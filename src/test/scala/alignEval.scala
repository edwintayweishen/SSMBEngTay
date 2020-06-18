object alignEval extends App{

  import scalismo.geometry._
  import scalismo.common._
  import scalismo.ui.api._
  import scalismo.io._
  import scalismo.mesh._


  scalismo.initialize()
  implicit val rng = scalismo.utils.Random(42)
  val ui = ScalismoUI()

  val target = "672_3837_F18_L_3D.obj"

  // load in target (reference mesh)

  val inputRef = LandmarkIO.readLandmarksCsv[_3D](new java.io.File("datasets/604_3745_14_L_3D.obj")).get
  val landmarks1 : Seq[Landmark[_3D]] = inputRef
  val refIndexSeq = landmarks1.map(I => I.point).toIndexedSeq
  val refIndexIds = landmarks1.map(I => PointId(I.id.toInt)).toIndexedSeq
  val refMesh : TriangleMesh[_3D] = TriangleMesh3D(refIndexSeq, TriangleList(IndexedSeq()))
  val group1 = ui.createGroup("Input Reference")
  val refView = ui.show(group1, refIndexSeq, "reference 'mesh'")

  val stl = MeshIO.readMesh(new java.io.File("stlmodel.stl")).get
  ui.show(stl,"stl")

  // load in pre/post alignment mesh

  val inputTarget = LandmarkIO.readLandmarksCsv[_3D](new java.io.File("ICPResults/" + target)).get
  val landmarks2 : Seq[Landmark[_3D]] = inputTarget
  val targetIndexSeq = landmarks2.map(I => I.point).toIndexedSeq
  val targetIndexIds = landmarks2.map(I => PointId(I.id.toInt)).toIndexedSeq
  val targetMesh : TriangleMesh[_3D] = TriangleMesh3D(targetIndexSeq, TriangleList(IndexedSeq()))
  val group2 = ui.createGroup("Input Target")
  val targetView = ui.show(group2, targetIndexSeq, "target 'mesh'")

  // eval average distance and hausdorff distance

  def averageDistance(m1: TriangleMesh[_3D], m2: TriangleMesh[_3D]): Double = {

    val dists = for(ptM1 <- m1.pointSet.points) yield {
      val cpM2 = m2.pointSet.findClosestPoint(ptM1).point
      (ptM1 - cpM2).norm
    }
    dists.sum / m1.pointSet.numberOfPoints
  }


  def hausDistance(m1: TriangleMesh[_3D], m2: TriangleMesh[_3D]) : Double = {
    def allDistsBetweenMeshes(mm1: TriangleMesh[_3D], mm2: TriangleMesh[_3D]) : Iterator[Double] = {
      for(ptM1 <- mm1.pointSet.points) yield {
        val cpM2 = mm2.pointSet.findClosestPoint(ptM1).point
        (ptM1 - cpM2).norm
      }
    }

    val d1 = allDistsBetweenMeshes(m1,m2)
    val d2 = allDistsBetweenMeshes(m2,m1)

    Math.max(d1.max,d2.max)

  }


  val aD = averageDistance(refMesh,targetMesh)

  println(aD)

  val hD = hausDistance(refMesh, targetMesh)

  print(hD)





}
