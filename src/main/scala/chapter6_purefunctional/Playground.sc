val rng = new scala.util.Random

rng.nextDouble()
rng.nextDouble()
rng.nextInt()
rng.nextInt(10) //0-9

def rollDie: Int = {
  val rng = new scala.util.Random
  rng.nextInt(6)
}