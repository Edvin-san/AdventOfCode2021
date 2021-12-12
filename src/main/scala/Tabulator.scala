// Taken from https://stackoverflow.com/a/7542476
object Tabulator {
  def format(table: Seq[Seq[Any]]) = table match {
    case Seq() => ""
    case _ =>
      val sizes = for (row <- table) yield (for (cell <- row) yield if (cell == null) 0 else removeSpecialAnsiColorStrings(cell.toString).length)
      val colSizes = for (col <- sizes.transpose) yield col.max
      val rows = for (row <- table) yield formatRow(row, colSizes)
      formatRows(rowSeparator(colSizes), rows)
  }

  def formatTable(table: Seq[Seq[Any]]): String = {
    if (table.isEmpty) ""
    else {
      // Get column widths based on the maximum cell width in each column (+2 for a one character padding on each side)
      val colWidths = table.transpose.map(_.map(cell => if (cell == null) 0 else cell.toString.length).max + 2)
      // Format each row
      val rows = table.map(_.zip(colWidths).map { case (item, size) => (" %-" + (size - 1) + "s").format(item) }
        .mkString("|", "|", "|"))
      // Formatted separator row, used to separate the header and draw table borders
      val separator = colWidths.map("-" * _).mkString("+", "+", "+")
      // Put the table together and return
      (separator +: rows.head +: separator +: rows.tail :+ separator).mkString("\n")
    }
  }

  private def removeSpecialAnsiColorStrings(s: String): String = List(
    Console.BLACK,
    Console.RED,
    Console.GREEN,
    Console.YELLOW,
    Console.BLUE,
    Console.MAGENTA,
    Console.CYAN,
    Console.WHITE,
    Console.BLACK_B,
    Console.RED_B,
    Console.GREEN_B,
    Console.YELLOW_B,
    Console.BLUE_B,
    Console.MAGENTA_B,
    Console.CYAN_B,
    Console.WHITE_B,
    Console.RESET,
    Console.BOLD,
    Console.UNDERLINED,
    Console.BLINK,
    Console.REVERSED,
    Console.INVISIBLE
  ).foldLeft(s) { case (acc, ansi) => acc.replace(ansi, "") }

  def formatRows(rowSeparator: String, rows: Seq[String]): String = (
    rowSeparator ::
      rows.head ::
      rowSeparator ::
      rows.tail.toList :::
      rowSeparator ::
      List()).mkString("\n")

  def formatRow(row: Seq[Any], colSizes: Seq[Int]) = {
    val cells = (for ((item, size) <- row.zip(colSizes)) yield if (size == 0) "" else ("%" + size + "s").format(item))
    cells.mkString("|", "|", "|")
  }

  def rowSeparator(colSizes: Seq[Int]) = colSizes map {
    "-" * _
  } mkString("+", "+", "+")
}