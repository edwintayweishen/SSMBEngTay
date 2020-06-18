object meshAlignEval extends App{

  import scalismo.geometry._
  import scalismo.common._
  import scalismo.ui.api._
  import scalismo.io._
  import scalismo.mesh._

  scalismo.initialize()
  implicit val rng = scalismo.utils.Random(42)

//  val target = "672_3837_F18_L_3D.vtk"

//  val reference = ""

  val refMesh = MeshIO.readMesh(new java.io.File("meshaligneval/regeval/" + "reference")).get

  val targetMesh = MeshIO.readMesh(new java.io.File("meshaligneval/regeval/" + "target")).get

  val avgDist = MeshMetrics.avgDistance(refMesh,targetMesh)
  println(avgDist)

  val hausDist = MeshMetrics.hausdorffDistance(refMesh,targetMesh)
  println(hausDist)


}
