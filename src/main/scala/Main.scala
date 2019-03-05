object Main extends App {

  case class ElevationWithPath(path: Int = 0, elevation: Int = 0)

  override def main(args: Array[String]): Unit = {

    var maxPath, maxElevation = 0

    val skiMatrix = scala.io.Source.fromURL("http://s3-ap-southeast-1.amazonaws.com/geeks.redmart.com/coding-problems/map.txt")
      .mkString.split("\n").slice(1, 1001).map(v => v.split(" ").map(_.toInt))

    val scoreMatrix = Array.fill[Option[ElevationWithPath]](skiMatrix.length, skiMatrix(0).length)(None)

    def updateMaxScore(oldScore: ElevationWithPath, newScore: ElevationWithPath): ElevationWithPath = {
      if (newScore.path > maxPath) {
        maxPath = newScore.path
        maxElevation = newScore.elevation
      } else if (newScore.path == maxPath && newScore.elevation > maxElevation) {
        maxElevation = newScore.elevation
      }
      ElevationWithPath(Math.max(newScore.path, oldScore.path),
        if (newScore.path >= oldScore.path) newScore.elevation else oldScore.elevation)
    }

    def findMaxInPath(x: Int, y: Int): ElevationWithPath = {

      if (x < 0 || y < 0 || x >= skiMatrix.length || y >= skiMatrix(0).length) return ElevationWithPath()

      var score = ElevationWithPath()

      scoreMatrix(x)(y) match {
        case Some(el) => el
        case None =>
          if (x > 0 && skiMatrix(x - 1)(y) < skiMatrix(x)(y)) {
            val topScore = findMaxInPath(x - 1, y)
            val newScore = ElevationWithPath(topScore.path + 1, topScore.elevation + skiMatrix(x)(y) - skiMatrix(x - 1)(y))
            score = updateMaxScore(score, newScore)
          }
          if (y < skiMatrix(0).length - 1 && skiMatrix(x)(y + 1) < skiMatrix(x)(y)) {
            val rightScore = findMaxInPath(x, y + 1)
            val newScore = ElevationWithPath(rightScore.path + 1, rightScore.elevation + skiMatrix(x)(y) - skiMatrix(x)(y + 1))
            score = updateMaxScore(score, newScore)
          }
          if (y > 0 && skiMatrix(x)(y - 1) < skiMatrix(x)(y)) {
            val leftScore = findMaxInPath(x, y - 1)
            val newScore = ElevationWithPath(leftScore.path + 1, leftScore.elevation + skiMatrix(x)(y) - skiMatrix(x)(y - 1))
            score = updateMaxScore(score, newScore)
          }
          if (x < skiMatrix.length - 1 && skiMatrix(x + 1)(y) < skiMatrix(x)(y)) {
            val downScore = findMaxInPath(x + 1, y)
            val newScore = ElevationWithPath(downScore.path + 1, downScore.elevation + skiMatrix(x)(y) - skiMatrix(x + 1)(y))
            score = updateMaxScore(score, newScore)
          }
          scoreMatrix(x)(y) = Some(score)
          score
      }
    }

    for (i <- skiMatrix.indices) {
      for (j <- skiMatrix(i).indices) {
        findMaxInPath(i, j)
      }
    }

    println(s"${maxPath + 1}$maxElevation")
  }

}
