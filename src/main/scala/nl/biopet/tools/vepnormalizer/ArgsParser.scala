/*
 * Copyright (c) 2014 Sequencing Analysis Support Core - Leiden University Medical Center
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

import nl.biopet.utils.tool.{AbstractOptParser, ToolCommand}

class ArgsParser(toolCommand: ToolCommand[Args])
    extends AbstractOptParser[Args](toolCommand) {
  head(
    s"""|${toolCommand.toolName} - Parse VEP-annotated VCF to standard VCF format """)

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
