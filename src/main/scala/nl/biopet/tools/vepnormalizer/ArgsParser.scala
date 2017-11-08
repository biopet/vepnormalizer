package nl.biopet.tools.vepnormalizer

import java.io.File

import nl.biopet.utils.tool.AbstractOptParser

class ArgsParser(cmdName: String) extends AbstractOptParser[Args](cmdName) {
  head(s"""|$cmdName - Parse VEP-annotated VCF to standard VCF format """)

  opt[File]('I', "InputFile") required () valueName "<vcf>" action { (x, c) =>
    c.copy(inputVCF = x)
  } validate { x =>
    if (x.exists) success else failure("Input VCF not found")
  } text "Input VCF file. Required."
  opt[File]('O', "OutputFile") required () valueName "<vcf>" action { (x, c) =>
    c.copy(outputVCF = x)
  } validate { x =>
    if (!x.getName.endsWith(".vcf") && (!x.getName.endsWith(".vcf.gz")) && (!x.getName
          .endsWith(".bcf")))
      failure("Unsupported output file type")
    else success
  } text "Output VCF file. Required."

  opt[String]('m', "mode") required () valueName "<mode>" action { (x, c) =>
    c.copy(mode = x)
  } validate { x =>
    if (x == "explode") success
    else if (x == "standard") success
    else failure("Unsupported mode")
  } text "Mode. Can choose between <standard> (generates standard vcf) and <explode> (generates new record for each transcript). Required."

  opt[Unit]("do-not-remove") action { (_, c) =>
    c.copy(removeCSQ = false)
  } text "Do not remove CSQ tag. Optional"
}
