package net.gumbix.dynpro

/**
 * @author Markus Gumbel (m.gumbel@hs-mannheim.de)
 */
trait DynProMatrix {
  /**
   * The number of rows of the matrix.
   */
  def n: Int

  /**
   * The number of columns of the matrix.
   */
  def m: Int

  /**
   * Matrix containing the accumulated values (costs).
   * A cell may be empty (=None).
   */
  val matrix: Array[Array[Option[Double]]]
}