/*
 * Copyright (c) 2014 Biopet
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal in
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
 * the Software, and to permit persons to whom the Software is furnished to do so,
 * subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
 * FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
 * COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
 * IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package nl.biopet.tools.vepnormalizer

import java.io.File

import htsjdk.tribble.TribbleException
import htsjdk.variant.vcf.VCFFileReader
import nl.biopet.utils.test.tools.ToolTest
import org.testng.annotations.Test

import scala.util.Random

class VepNormalizerTest extends ToolTest[Args] {
  def toolCommand: VepNormalizer.type = VepNormalizer

  import VepNormalizer._

  @Test
  def testNoArgs(): Unit = {
    intercept[IllegalArgumentException] {
      VepNormalizer.main(Array())
    }
  }

  val vcf3 = new File(resourcePath("/VCFv3.vcf"))
  val vepped = new File(resourcePath("/VEP_oneline.vcf"))
  val unvepped = new File(resourcePath("/unvepped.vcf"))

  val veppedPath: String = resourcePath("/VEP_oneline.vcf")

  val rand = new Random()

  @Test def testGzOutputExplode(): Unit = {
    val tmpFile = File.createTempFile("VepNormalizer_", ".vcf.gz")
    tmpFile.deleteOnExit()
    val arguments: Array[String] =
      Array("-I", veppedPath, "-O", tmpFile.getAbsolutePath, "-m", "explode")
    main(arguments)
  }

  @Test def testVcfOutputExplode(): Unit = {
    val tmpFile = File.createTempFile("VepNormalizer_", ".vcf")
    tmpFile.deleteOnExit()
    val arguments: Array[String] =
      Array("-I", veppedPath, "-O", tmpFile.getAbsolutePath, "-m", "explode")
    main(arguments)
  }

  @Test def testBcfOutputExplode(): Unit = {
    val tmpFile = File.createTempFile("VepNormalizer_", ".bcf")
    tmpFile.deleteOnExit()
    val arguments: Array[String] =
      Array("-I", veppedPath, "-O", tmpFile.getAbsolutePath, "-m", "explode")
    main(arguments)
  }

  @Test def testGzOutputStandard(): Unit = {
    val tmpFile = File.createTempFile("VepNormalizer_", ".vcf.gz")
    tmpFile.deleteOnExit()
    val arguments: Array[String] =
      Array("-I", veppedPath, "-O", tmpFile.getAbsolutePath, "-m", "standard")
    main(arguments)
  }

  @Test def testVcfOutputStandard(): Unit = {
    val tmpFile = File.createTempFile("VepNormalizer_", ".vcf")
    tmpFile.deleteOnExit()
    val arguments: Array[String] =
      Array("-I", veppedPath, "-O", tmpFile.getAbsolutePath, "-m", "standard")
    main(arguments)
  }

  @Test def testBcfOutputStandard(): Unit = {
    val tmpFile = File.createTempFile("VepNormalizer_", ".bcf")
    tmpFile.deleteOnExit()
    val arguments: Array[String] =
      Array("-I", veppedPath, "-O", tmpFile.getAbsolutePath, "-m", "standard")
    main(arguments)
  }

  @Test def testVEPHeaderLength(): Unit = {
    val reader = new VCFFileReader(vepped, false)
    val header = reader.getFileHeader
    parseCsq(header).length should be(27)
  }

  @Test def testExplodeVEPLength(): Unit = {
    val reader = new VCFFileReader(vepped, false)
    val header = reader.getFileHeader
    val newInfos = parseCsq(header)
    explodeTranscripts(reader.iterator().next(), newInfos, removeCsq = true).length should be(
      11)
  }

  @Test def testStandardVEPLength(): Unit = {
    val reader = new VCFFileReader(vepped, false)
    val header = reader.getFileHeader
    val newInfos = parseCsq(header)
    Array(standardTranscripts(reader.iterator().next(),
                              newInfos,
                              removeCsq = true)).length should be(1)
  }

  @Test def testStandardVEPAttributeLength(): Unit = {
    val reader = new VCFFileReader(vepped, false)
    val header = reader.getFileHeader
    val newInfos = parseCsq(header)
    val record =
      standardTranscripts(reader.iterator().next(), newInfos, removeCsq = true)
    def checkItems(items: Array[String]): Unit = {
      items.foreach { check }
    }

    def check(item: String): Unit = {
      record.getAttribute(item) match {
        case l: List[_] => l.length should be(11)
        case _          =>
      }
    }

    val items = Array(
      "AA_MAF",
      "AFR_MAF",
      "ALLELE_NUM",
      "AMR_MAF",
      "ASN_MAF",
      "Allele",
      "Amino_acids",
      "CDS_position",
      "CLIN_SIG",
      "Codons",
      "Consequence",
      "DISTANCE",
      "EA_MAF",
      "EUR_MAF",
      "Existing_variation",
      "Feature",
      "Feature_type",
      "GMAF",
      "Gene",
      "HGVSc",
      "HGVSp",
      "PUBMED",
      "Protein_position",
      "STRAND",
      "SYMBOL",
      "SYMBOL_SOURCE",
      "cDNA_position"
    ).map("VEP_" + _)

    checkItems(items)
  }

  @Test
  def testVCF3TribbleException(): Unit = {
    intercept[TribbleException.MalformedFeatureFile] {
      new VCFFileReader(vcf3, false)
    }
  }

  @Test
  def testNoCSQTagException() {
    intercept[IllegalArgumentException] {
      csqCheck(new VCFFileReader(unvepped, false).getFileHeader)
    }
  }
}
