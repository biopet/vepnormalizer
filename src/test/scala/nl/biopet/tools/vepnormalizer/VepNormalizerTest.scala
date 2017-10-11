package nl.biopet.tools.vepnormalizer

import nl.biopet.test.BiopetTest
import org.testng.annotations.Test

object VepNormalizerTest extends BiopetTest {
  @Test
  def testNoArgs(): Unit = {
    intercept[IllegalArgumentException] {
      VepNormalizer.main(Array())
    }
  }
}
