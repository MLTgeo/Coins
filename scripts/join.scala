
import io.Source
import java.io._


val fw = new BufferedWriter(new FileWriter(new File(args(1))))

new File(args(0)).listFiles.foreach {
  f =>
    val s = Source.fromFile(f) 
    s.getLines.foreach {
      l => 
        fw.append(l)
        fw.append("\n")
    }
    s.close
}

fw.close

