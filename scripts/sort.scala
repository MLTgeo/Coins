
import io.Source
import java.io._

val lines = 
  Source.fromFile(args(0)).getLines.map{_.split(",")}.toList.
    sortBy(_(3).toDouble).
    sortBy(_(4).toDouble).
    sortBy(_(2).toDouble).
    sortBy(_(1).toDouble).
    sortBy(_(0).toDouble)

val fw = new BufferedWriter(new FileWriter(new File(args(1))))

lines.foreach {
  l => 
    fw.append(l.mkString(","))
    fw.append("\n")
}

