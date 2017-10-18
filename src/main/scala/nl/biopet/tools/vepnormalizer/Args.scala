package nl.biopet.tools.vepnormalizer

import java.io.File

case class Args(inputVCF: File = null,
                outputVCF: File = null,
                mode: String = null,
                removeCSQ: Boolean = true)
