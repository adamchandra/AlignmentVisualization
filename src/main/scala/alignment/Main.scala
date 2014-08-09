package alignment

import D3._
import scala.scalajs.js
import scala.scalajs.js.annotation.{JSName, JSExport}
import org.scalajs.dom
import D3.Layout.{GraphNode, GraphLink}
import scala.collection._
import org.scalajs.jquery.jQuery
import scala.Array

@JSExport
object AlignmentVisualize extends js.JSApp {

  import Util._

  def main(): Unit = {
    def getSequene(sel: String): Array[Char] = """\s""".r.replaceAllIn(jQuery(sel).value().asInstanceOf[String], "").toCharArray

    jQuery("#docalc").click(() => {
      jQuery("#result").value("")
      val method: String = jQuery("#method").value().asInstanceOf[String]
      val s1 = getSequene("#s1")
      val s2 = getSequene("#s2")
      if (validate(s1) && validate(s2)) {
        val r = method match {
          case "lcs" => new LCS(s1, s2).calc
          case "needleman" => new Needleman(s1, s2).calc
          case "smith" => new Smith(s1, s2).calc
        }
        jQuery("#result").value(r.str.mkString("\n"))
        drawGraph(r, s1, s2)
      } else {
        dom.window.alert("Invalid input.")
      }
    })
    jQuery("button.method").click(() => {

    })
    jQuery("#docalc").click()
  }

  def validate(cs: Array[Char]): Boolean = cs.forall(c => "ATGC".contains(c))

  def drawGraph(r: Result, s1: Array[Char], s2: Array[Char]) {
    val d3 = D3Obj.d3
    val interval = 15
    val size = 12

    val scale = 2

    val width: js.Number = scale * ((s2.length + 1) * interval) + 100
    val height: js.Number = scale * ((s1.length + 1) * interval) + 50

    jQuery("svg").remove()

    val svg = d3.select("#svgdiv").append("svg")
      .attr("width", width)
      .attr("height", height)

    val cells = r.cells.flatten

    val rects = svg.append("g")
      .attr("transform", s"scale($scale)")
      .attr("class", "rects")
      .selectAll("g")
      .data(js.Array(cells: _*))
      .enter()
      .append("g")

    def xi(i: Int): Int = i % (s2.length + 1)
    def yi(i: Int): Int = math.floor(i / (s2.length + 1)).toInt

    def xi2(i: Int): Int = i % (s2.length + 1)
    def yi2(i: Int): Int = math.floor(i / (s2.length + 1)).toInt

    def color(ov: Option[Int],min: Int, max: Int): String = {ov match {
      case Some(v) => d3.hsl((max-v)*240/(max-min),0.7,0.5).toString // d3.scale.linear.range(js.Array(0d,1d*v)).domain(js.Array("blue","red"))(v)
      case None => "gray"
    }}

    val mx = cells.flatten.max
    val mn = cells.flatten.min
    val rects2 = rects.append("rect")
      .style("fill", (ov: Option[Int], i: js.Number) => color(ov,mn,mx).asInstanceOf[js.Dynamic])
      .attr("x", (ov: Option[Int], i: js.Number) => xi(i.toInt) * interval + 20)
      .attr("y", (ov: Option[Int], i: js.Number) => yi(i.toInt) * interval + 20)
      .attr("width", (ov: Option[Int]) => size)
      .attr("height", (ov: Option[Int]) => size)
      .style("opacity", (ov: Option[Int], i: js.Number) => 1.asInstanceOf[js.Dynamic])


    val pathInterval = math.min(300,3000 / r.path.length)
    val delays: Array[Double] = Array.tabulate(cells.length){i =>
      val idx = r.path.indexWhere(a => {
        val (yy, xx, _, _) = a
        yy == yi(i) && xx == xi(i)
      })
      if(idx == -1){
        0d
      }else{
        300+idx*pathInterval
      }
    }
    rects2.transition()
      .delay((ov: Any, i: js.Number) => delays(i.toInt))
      .attr("class", (ov: Option[Int], i: js.Number) => {
      val x = xi(i.toInt)
      val y = yi(i.toInt)
      if (r.path.exists(a => {
        val (yy, xx, _, _) = a
        yy == y && xx == x
      })) {
        "on-path"
      } else {
        ""
      }
    })

    val scores = rects.append("text")
      .attr("class", "score")
      .attr("x", (ov: Option[Int], i: js.Number) => xi(i.toInt) * interval + 27)
      .attr("y", (ov: Option[Int], i: js.Number) => yi(i.toInt) * interval + 28)
      .text((ov: Option[Int], i: js.Number) => ov.getOrElse("").toString)
      .attr("text-anchor","middle")


    val arrows = rects.append("text")
      .attr("x", (ov: Option[Int], i: js.Number) => xi(i.toInt) * interval + 23)
      .attr("y", (ov: Option[Int], i: js.Number) => yi(i.toInt) * interval + 30)
      .attr("text-anchor","middle")

    def textcolor(m: Array[Matching.Value], i: Int): String = m(i) match {
      case Matching.Match => "blue"
      case Matching.Mismatch => "#FF2A82"
      case Matching.Missing => "gray"
    }

    val axisx = svg.append("g").attr("class", "axis")
      .attr("transform", s"scale($scale)")
      .selectAll("text")
      .data(js.Array(s2: _*))
      .enter()
      .append("text")
      .attr("x", (ov: Char, i: js.Number) => (i + 1) * interval + 22)
      .attr("y", 15)
      .style("fill", (ov: Char, i: js.Number) => textcolor(r.match2, i.toInt),"")
      .text((v: Char, i: js.Number) => v.toString)

    val axisy = svg.append("g").attr("class", "axis")
      .attr("transform", s"scale($scale)")
      .selectAll("text")
      .data(js.Array(s1: _*))
      .enter()
      .append("text")
      .attr("y", (ov: Char, i: js.Number) => (i + 1) * interval + 30)
      .attr("x", 10)
      .style("fill", (ov: Char, i: js.Number) => textcolor(r.match1, i.toInt),"")
      .text((v: Char, i: js.Number) => v.toString)

    rects.data(cells).attr("class", (ov: Option[Int], i: js.Number) => if (ov.isDefined) "done" else "")
    scores.data(cells)
    val arr: Array[Option[Direction.Value]] = r.arrows.flatten.toArray
    arrows.data(arr)
      .attr("opacity", (ov: Option[Int], i: js.Number) => if (ov.isDefined) 1 else 0)
      .attr("class", (ov: Option[Direction.Value], i: js.Number) => {
      ov match {
        case Some(Direction.Left) => "arrow left"
        case Some(Direction.Top) => "arrow top"
        case Some(Direction.LeftTop) => "arrow lefttop"
        case _ => "none"
      }
    }).attr("x", (ov: Option[Direction.Value], i: js.Number) => {
      xi2(i.toInt) * interval + 23 + (ov match {
        case Some(Direction.Left) => -4
        case Some(Direction.Top) => 3
        case Some(Direction.LeftTop) => -4
        case _ => 0
      })
    }).attr("y", (ov: Option[Direction.Value], i: js.Number) => {
      yi2(i.toInt) * interval + 23 + (ov match {
        case Some(Direction.Left) => 3
        case Some(Direction.Top) => -4
        case Some(Direction.LeftTop) => -4
        case _ => 0
      })
    }).text( (ov: Option[Direction.Value], i: js.Number) => {
      ov match {
        case Some(Direction.Left) => "←"
        case Some(Direction.Top) => "↑"
        case Some(Direction.LeftTop) => "↖"
        case _ => ""
      }})

    scores.text((ov: Option[Int], i: js.Number) => ov.getOrElse("").toString)
  }



}
