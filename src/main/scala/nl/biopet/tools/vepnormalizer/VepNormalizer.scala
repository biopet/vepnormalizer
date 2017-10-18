package nl.biopet.tools.vepnormalizer

import htsjdk.tribble.TribbleException
import htsjdk.variant.variantcontext.writer.{AsyncVariantContextWriter, VariantContextWriterBuilder}
import htsjdk.variant.variantcontext.{VariantContext, VariantContextBuilder}
import htsjdk.variant.vcf._
import nl.biopet.utils.tool.ToolCommand

import scala.collection.JavaConversions._

object VepNormalizer extends ToolCommand {
  def main(args: Array[String]): Unit = {
    val parser = new ArgsParser(toolName)
    val cmdArgs =
      parser.parse(args, Args()).getOrElse(throw new IllegalArgumentException)

    logger.info("Start")

    val input = cmdArgs.inputVCF
    val output = cmdArgs.outputVCF

    logger.info(s"""Input VCF is $input""")
    logger.info(s"""Output VCF is $output""")

    val reader = try {
      new VCFFileReader(input, false)
    } catch {
      case e: TribbleException.MalformedFeatureFile =>
        logger.error("Malformed VCF file! Are you sure this isn't a VCFv3 file?")
        throw e
    }

    val header = reader.getFileHeader
    val writer = new AsyncVariantContextWriter(
      new VariantContextWriterBuilder()
        .setOutputFile(output)
        .setReferenceDictionary(header.getSequenceDictionary)
        build())

    if (reader.iterator().hasNext) {
      logger.debug("Checking for CSQ tag")
      csqCheck(header)
      logger.debug("CSQ tag OK")
      logger.debug("Checkion VCF version")
      versionCheck(header)
      logger.debug("VCF version OK")
      logger.debug("Parsing header")
      val newInfos = parseCsq(header)
      header.setWriteCommandLine(true)

      for (info <- newInfos) {
        val tmpheaderline = new VCFInfoHeaderLine(info,
          VCFHeaderLineCount.UNBOUNDED,
          VCFHeaderLineType.String,
          "A VEP annotation")
        header.addMetaDataLine(tmpheaderline)
      }
      logger.debug("Header parsing done")

      logger.debug("Writing header to file")

      writer.writeHeader(header)
      logger.debug("Wrote header to file")

      normalize(reader, writer, newInfos, cmdArgs.mode, cmdArgs.removeCSQ)
    } else {
      logger.debug("No variants found, skipping normalize step")
      writer.writeHeader(header)
    }
    writer.close()
    logger.debug("Closed writer")
    reader.close()
    logger.debug("Closed reader")
    logger.info("Done")
  }

  /**
    * Normalizer
    *
    * @param reader    input VCF VCFFileReader
    * @param writer    output VCF AsyncVariantContextWriter
    * @param newInfos  array of string containing names of new info fields
    * @param mode      normalizer mode (explode or standard)
    * @param removeCsq remove csq tag (Boolean)
    * @return
    */
  def normalize(reader: VCFFileReader,
                writer: AsyncVariantContextWriter,
                newInfos: Array[String],
                mode: String,
                removeCsq: Boolean): Unit = {
    logger.info(s"""You have selected mode $mode""")
    logger.info("Start processing records")

    var counter = 0
    for (record <- reader) {
      mode match {
        case "explode" =>
          explodeTranscripts(record, newInfos, removeCsq).foreach(vc => writer.add(vc))
        case "standard" => writer.add(standardTranscripts(record, newInfos, removeCsq))
        case _ => throw new IllegalArgumentException("Something odd happened!")
      }
      counter += 1
      if (counter % 100000 == 0) logger.info(counter + " variants processed")
    }
    logger.info("done: " + counter + " variants processed")
  }

  /**
    * Checks whether header has a CSQ tag
    *
    * @param header VCF header
    */
  def csqCheck(header: VCFHeader): Unit = {
    if (!header.hasInfoLine("CSQ")) {
      //logger.error("No CSQ info tag found! Is this file VEP-annotated?")
      throw new IllegalArgumentException("No CSQ info tag found! Is this file VEP-annotated?")
    }
  }

  /**
    * Checks whether version of input VCF is at least 4.0
    * VEP is known to cause issues below 4.0
    * Throws exception if not
    *
    * @param header VCFHeader of input VCF
    */
  def versionCheck(header: VCFHeader): Unit = {
    var format = ""
    //HACK: getMetaDataLine does not work for fileformat
    for (line <- header.getMetaDataInInputOrder) {
      if (line.getKey == "fileformat" || line.getKey == "format") {
        format = line.getValue
      }
    }
    val version = VCFHeaderVersion.toHeaderVersion(format)
    if (!version.isAtLeastAsRecentAs(VCFHeaderVersion.VCF4_0)) {
      throw new IllegalArgumentException(s"""version $version is not supported""")
    }
  }

  /**
    * Parses the CSQ tag in the header
    *
    * @param header the VCF header
    * @return list of strings with new info fields
    */
  def parseCsq(header: VCFHeader): Array[String] = {
    header.getInfoHeaderLine("CSQ").getDescription.split(':')(1).trim.split('|').map("VEP_" + _)
  }

  /**
    * Explode a single VEP-annotated record to multiple normal records
    * Based on the number of annotated transcripts in the CSQ tag
    *
    * @param record   the record as a VariantContext object
    * @param csqInfos An array with names of new info tags
    * @return An array with the new records
    */
  def explodeTranscripts(record: VariantContext,
                         csqInfos: Array[String],
                         removeCsq: Boolean): Array[VariantContext] = {
    for (transcript <- parseCsq(record)) yield {
      (for (fieldId <- csqInfos.indices if transcript.isDefinedAt(fieldId);
            value = transcript(fieldId) if value.nonEmpty) yield csqInfos(fieldId) -> value)
        .filterNot(_._2.isEmpty)
        .foldLeft(createBuilder(record, removeCsq))((builder, attribute) =>
          builder.attribute(attribute._1, attribute._2))
        .make()
    }
  }

  def standardTranscripts(record: VariantContext,
                          csqInfos: Array[String],
                          removeCsq: Boolean): VariantContext = {
    val attribs = parseCsq(record)

    (for (fieldId <- csqInfos.indices)
      yield
        csqInfos(fieldId) -> {
          for (transcript <- attribs if transcript.isDefinedAt(fieldId);
               value = transcript(fieldId) if value.nonEmpty) yield value
        })
      .filter(_._2.nonEmpty)
      .foldLeft(createBuilder(record, removeCsq))((builder, attribute) =>
        builder.attribute(attribute._1, attribute._2))
      .make()
  }

  def createBuilder(record: VariantContext, removeCsq: Boolean): VariantContextBuilder = {
    if (removeCsq) new VariantContextBuilder(record).rmAttribute("CSQ")
    else new VariantContextBuilder(record)
  }

  def parseCsq(record: VariantContext): Array[Array[String]] = {
    record
      .getAttributeAsString("CSQ", "unknown")
      .stripPrefix("[")
      .stripSuffix("]")
      .split(",")
      .map(_.split("""\|""").map(_.trim))
  }
}
