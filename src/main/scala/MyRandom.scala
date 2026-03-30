trait MyRandom:
  def nextInt(): (Int, MyRandom)
  def nextInt(n: Int): (Int, MyRandom) =
    val (i, r) = nextInt()
    (Math.abs(i) % n, r)

case class PredictableRandom(seed: Long) extends MyRandom:
  def nextInt(): (Int, MyRandom) =
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextValue = (newSeed >>> 16).toInt
    (nextValue, PredictableRandom(newSeed))