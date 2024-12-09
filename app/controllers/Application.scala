package controllers

import javax.inject._
import play.api._
import play.api.db.Database
import play.api.mvc._
import play.api.libs.json._
import scala.math._
import scala.collection.mutable.ArrayBuffer

class Complex(val x: Double, val y: Double) {
  val magSq = x * x + y * y
  var iterNo: Int = 0
  def setIterNo(newIterNo: Int): Unit = {
    iterNo = newIterNo
  }
  def add(second: Complex): Complex = {
    val z = new Complex(x + second.x, y + second.y)
    z.setIterNo(iterNo)
    z
  }
  def mul(second: Complex): Complex = {
    val xb = second.x
    val yb = second.y
    val z = new Complex(x * xb - y * yb, x * yb + y * xb)
    z.setIterNo(iterNo)
    z
  }
  def iter(second: Complex): Complex = {
    val xNew = second.x * second.x - second.y * second.y + x
    val yNew = (second.x + second.x) * second.y + y
    new Complex(xNew, yNew)
  }
  def calcIterNo(maxIter: Int): Int = {
    var n = 0
    var z = new Complex(0, 0)
    while (n < maxIter && z.magSq < 4) {
      z = iter(z)
      n += 1
    }
    if (n == maxIter) 0 else n
  }
  def color(maxIterNo: Int): String = {
    if (iterNo == 0) {
      "#000000"
    } else {
      val n = floor(255 * iterNo / maxIterNo).toInt
      var hexR = (if (n < 16) "0" else "") + Integer.toString(n, 16)
      var hexB = (if (255 - n < 16) "0" else "") + Integer.toString(255 - n, 16)
      s"#${hexR}00${hexB}"
    }
  }
}

class Grid(val size: Double, val nxOverTwo: Int, val maxIter: Int, val mag: Int, val c: Complex) {
  val mag2 = {
    pow(2, mag)
  }
  val rows = {
    var dxTimesTwo = 2.0 / nxOverTwo
    var dy = dxTimesTwo * sqrt(3) / 2
    val ny = floor(2.0 / dy).toInt
    var iy = -ny
    dxTimesTwo /= pow(2, mag).toDouble
    val dx = dxTimesTwo / 2
    dy /= pow(2, mag).toDouble
    val rows: ArrayBuffer[Array[Complex]] = ArrayBuffer()
    var numCells = 0
    while (iy <= ny) {
      val row: ArrayBuffer[Complex] = ArrayBuffer()
      val isEven = iy % 2 == 0
      var y = iy * dy
      var nx = floor(sqrt(4.0 / pow(2, 2 * mag).toDouble - y * y) / dx).toInt
      y += c.y
      if ((nx % 2 == 0) != isEven) nx -= 1
      var ix = -nx
      while (ix <= nx) {
        numCells += 1
        val x = ix * dx + c.x
        var z = new Complex(x, y)
        z.setIterNo(z.calcIterNo(maxIter))
        row += toDom(z)
        ix += 2
      }
      rows += row.toArray
      iy += 1
    }
    rows.toArray
  }
  val numberOfCells = {
    rows.map(_.length).sum
  }
  val maxIterNo = {
    var maxIterNo = 0
    for (row <- rows) {
      for (point <- row) {
        if (maxIterNo < point.iterNo) maxIterNo = point.iterNo
      }
    }
    maxIterNo
  }
  def toDom(z: Complex): Complex = {
    z.add(new Complex(-c.x, -c.y)).mul(new Complex(pow(2, mag).toDouble, 0.0)).add(new Complex(2.0, 2.0)).mul(new Complex(size / 2, 0))
  }
  def fromDom(z: Complex): Complex = {
    z.mul(new Complex(2 / size, 0)).add(new Complex(-2.0, -2.0)).mul(new Complex(pow(2, -mag).toDouble, 0.0)).add(new Complex(c.x, c.y))
  }
}

class Url(val nxOverTwoStr: String, val maxIterStr: String, val magStr: String, val cStr: String) {
  var nxOverTwo = 0
  var maxIter = 0
  var mag = 0
  var x = 0.0
  var y = 0.0
  private val error0 = "The url fragment "
  private val error1 = " cannot be parsed as "
  private val errorInteger = error1 + " an integer."
  private val errorNumber = error1 + " a number."
  private var messages: ArrayBuffer[String] = ArrayBuffer()
  def getMessages(): Array[String] = {
    try {
      nxOverTwo = nxOverTwoStr.toInt
    } catch {
      case e: NumberFormatException => messages += error0 + nxOverTwoStr + errorInteger
    }
    try {
      maxIter = maxIterStr.toInt
    } catch {
      case e: NumberFormatException => messages += error0 + maxIterStr + errorInteger
    }
    try {
      mag = magStr.toInt
    } catch {
      case e: NumberFormatException => messages += error0 + magStr + errorInteger
    }
    if (messages.isEmpty) {
      val cArr = cStr.replaceAll("\\s+", "").split(",")
      val xStr = cArr(0)
      if (cArr.length != 2) {
        messages += "The center " + cStr + " seems to have " + cArr.length.toString + " coordinate(s) instead of 2."
      } else {
        try {
          x = xStr.toDouble
        } catch {
          case e: NumberFormatException => messages += error0 + xStr + errorNumber
        }
        val yStr = cArr(1)
        try {
          y = yStr.toDouble
        } catch {
          case e: NumberFormatException => messages += error0 + yStr + errorNumber
        }
      }
    }
    messages.toArray
  }
}

@Singleton
class Application @Inject()(val controllerComponents: ControllerComponents, val database: Database) extends BaseController {

  def index(): Action[AnyContent] = Action { implicit request: Request[AnyContent] =>
    Ok(views.html.index())
  }

  def results(nxOverTwoStr: String, maxIterStr: String, magStr: String, cStr: String): Action[AnyContent] = Action { implicit request: Request[AnyContent] =>
    val url = new Url(nxOverTwoStr, maxIterStr, magStr, cStr)
    val messages = url.getMessages()
    if (!messages.isEmpty) {
      BadRequest(views.html.error(messages))
    } else {
      Ok(views.html.results(new Grid(
        400.0,
        url.nxOverTwo,
        url.maxIter,
        url.mag,
        new Complex(url.x, url.y),
      )))
    }
  }

  def jsonResults(nxOverTwoStr: String, maxIterStr: String, magStr: String, cStr: String): Action[AnyContent] = Action { implicit request: Request[AnyContent] =>
    val url = new Url(nxOverTwoStr, maxIterStr, magStr, cStr)
    val messages = url.getMessages()
    if (!messages.isEmpty) {
      BadRequest(Json.toJson(Map("errors" -> messages)))
    } else {
      val rows = (new Grid(
        400.0,
        url.nxOverTwo,
        url.maxIter,
        url.mag,
        new Complex(url.x, url.y),
      )).rows

      val rowsJson = Json.obj(
        "rows" -> Json.arr(rows.map(row => Json.arr(row.map(z => {
          Json.obj(
            "x" -> z.x,
            "y" -> z.y,
            "magSq" -> z.magSq,
            "iterNo" -> z.iterNo,
            "color" -> z.color(url.maxIter),
          )
        }))))
      )
      Ok(Json.toJson(Map("rows" -> rowsJson)))
    }
  }

  def db(): Action[AnyContent] = Action { implicit request: Request[AnyContent] =>
    // In this getting started app, we don't use a custom execution context to keep the code and configuration simple.
    // For real-world apps, consult the Play documentation on how to configure custom contexts and how to use them:
    // https://www.playframework.com/documentation/2.8.19/AccessingAnSQLDatabase#Using-a-CustomExecutionContext
    database.withConnection { connection =>
      val statement = connection.createStatement()
      statement.executeUpdate("CREATE TABLE IF NOT EXISTS ticks (tick timestamp)")
      statement.executeUpdate("INSERT INTO ticks VALUES (now())")

      val output = new StringBuilder();
      val resultSet = statement.executeQuery("SELECT tick FROM ticks")
      while (resultSet.next()) {
        output.append("Read from DB: " + resultSet.getTimestamp("tick") + "\n")
      }

      Ok(output.toString())
    }
  }
}
