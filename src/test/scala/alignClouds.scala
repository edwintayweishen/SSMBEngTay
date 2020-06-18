object alignClouds extends App {

  import scalismo.geometry._
  import scalismo.common._
  import scalismo.ui.api._
  import scalismo.mesh._
  import scalismo.io._
  import scalismo.registration._

  // initialize scalismo UI

  scalismo.initialize()
  implicit val rng = scalismo.utils.Random(42)
  val ui = ScalismoUI()


  val target = "587_3685_16_L_3D.obj"


  // Load in reference mesh, colour is default white, defined 'dataset 1' as group in the UI

  val inputRef = LandmarkIO.readLandmarksCsv[_3D](new java.io.File("datasets/604_3745_14_L_3D.obj")).get
  val landmarks1 : Seq[Landmark[_3D]] = inputRef
  val refIndexSeq = landmarks1.map(I => I.point).toIndexedSeq
  val refIndexIds = landmarks1.map(I => PointId(I.id.toInt)).toIndexedSeq
  val refMesh : TriangleMesh[_3D] = TriangleMesh3D(refIndexSeq, TriangleList(IndexedSeq()))
  val group1 = ui.createGroup("Dataset 1")
  val refView = ui.show(group1, refIndexSeq, "inputMesh")
  refView.radius = 0.8

  // Load in target mesh, colour is set to red, defined 'dataset 2' as group in UI

  val inputTarget = LandmarkIO.readLandmarksCsv[_3D](new java.io.File("datasets/" + target)).get
  val landmarks2 : Seq[Landmark[_3D]] = inputTarget
  val targetIndexSeq = landmarks2.map(I => I.point).toIndexedSeq
  val targetIndexIds = landmarks2.map(I => PointId(I.id.toInt)).toIndexedSeq
  val targetMesh : TriangleMesh[_3D] = TriangleMesh3D(targetIndexSeq, TriangleList(IndexedSeq()))
  val group2 = ui.createGroup("Dataset 2")
  val targetView = ui.show(group2, targetIndexSeq, "targetMesh")
  targetView.color = java.awt.Color.RED
  targetView.radius = 0.8

  // Selects a number of points from the reference point cloud to act as points to initialize alignment

  val pts = (0 until targetMesh.pointSet.numberOfPoints by 50).map(i => PointId(i))
  val chosenPoints = ui.show(group2, pts.map(id => targetMesh.pointSet.point(id)), "selected") // UNCOMMENT LINE 32 & 33 TO VISUALIZE CHOSEN POINTS
  chosenPoints.color = java.awt.Color.BLUE


  def attributeCorrespondences(movingMesh: TriangleMesh[_3D], ptIds : Seq[PointId]) : Seq[(Point[_3D], Point[_3D])] = {
    ptIds.map{ id: PointId =>
      val pt = movingMesh.pointSet.point(id)
      val closestPointOnRef = refMesh.pointSet.findClosestPoint(pt).point
      (pt, closestPointOnRef)
    }
  }


  // define iterative function

  def ICPRigidAlign(movingMesh: TriangleMesh[_3D], ptIds : Seq[PointId], numberOfIterations : Int) : TriangleMesh[_3D] = {
    if (numberOfIterations == 0)
      movingMesh
    else {
      val correspondences = attributeCorrespondences(movingMesh, ptIds)
      val transform = LandmarkRegistration.rigid3DLandmarkRegistration(correspondences, center = Point(0,0,0))
      val transformed = movingMesh.transform(transform)

      ICPRigidAlign(transformed, ptIds, numberOfIterations - 1)
    }
  }


  // Initializes the iterative function to begin rigid ICP of target to reference

  val rigidFit = ICPRigidAlign(targetMesh, targetIndexIds, 50)
  val rigidFitPoints = rigidFit.pointSet
  val rigidFitView = ui.show(group2, rigidFitPoints, "ICP_rigidFit")
  rigidFitView.color = java.awt.Color.GREEN


//   Converts result from 'mesh' back to IndexedSeq of points & IDs, then writes a new file

//  val resultPoints = rigidFit.pointSet.points.toIndexedSeq
//  val newCloud = for ((point, id) <- resultPoints.zipWithIndex) yield {
//    Landmark(s"$id", point)
//  }
//
//  LandmarkIO.writeLandmarksCsv(newCloud, new java.io.File("ICPResults/" + target)).get

// UNCOMMENT TO WRITE TO FILE
}
