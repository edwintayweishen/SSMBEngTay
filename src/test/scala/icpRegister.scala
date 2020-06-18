object icpRegister extends App {

  import scalismo.geometry._
  import scalismo.common._
  import scalismo.ui.api._
  import scalismo.mesh._
  import scalismo.io._
  import scalismo.statisticalmodel._
  import breeze.linalg.{DenseMatrix,DenseVector}
  import scalismo.kernels._

  // initialize scalismo UI

  scalismo.initialize()
  implicit val rng = scalismo.utils.Random(42)
  val ui = ScalismoUI()


  val target = "672_3837_F18_L_3D.obj"


  // load in 'targets'

  val inputTarget = LandmarkIO.readLandmarksCsv[_3D](new java.io.File("ICPResults/" + target)).get
  val landmarks2 : Seq[Landmark[_3D]] = inputTarget
  val targetIndexSeq = landmarks2.map(I => I.point).toIndexedSeq
  val targetIndexIds = landmarks2.map(I => PointId(I.id.toInt)).toIndexedSeq
  val targetMesh : TriangleMesh[_3D] = TriangleMesh3D(targetIndexSeq, TriangleList(IndexedSeq()))
  val group2 = ui.createGroup("Dataset 2")
  val targetView = ui.show(group2, targetIndexSeq, "targetMesh")
  targetView.color = java.awt.Color.RED
  targetView.radius = 0.8

  // load in reference cloud

  val inputRef = LandmarkIO.readLandmarksCsv[_3D](new java.io.File("datasets/604_3745_14_L_3D.obj")).get
  val landmarks1 : Seq[Landmark[_3D]] = inputRef
  val refIndexSeq = landmarks1.map(I => I.point).toIndexedSeq
  val refIndexIds = landmarks1.map(I => PointId(I.id.toInt)).toIndexedSeq
  val refMesh : TriangleMesh[_3D] = TriangleMesh3D(refIndexSeq, TriangleList(IndexedSeq()))
  val group1 = ui.createGroup("Dataset 1")
  val refView = ui.show(group1, refIndexSeq, "inputMesh")
  refView.radius = 0.8

  // define GP based SSM from reference 'mesh'

  val zeroMean = Field(RealSpace[_3D], (pt:Point[_3D]) => EuclideanVector(0,0,0))
  val scalarValuedGaussianKernel : PDKernel[_3D] = GaussianKernel(sigma = 100.0)
  val matrixValuedGaussianKernel = DiagonalKernel(scalarValuedGaussianKernel,3)
  val gp = GaussianProcess(zeroMean, matrixValuedGaussianKernel)

  val lowRankGP = LowRankGaussianProcess.approximateGPCholesky(
    refMesh.pointSet,
    gp,
    relativeTolerance = 0.01,
    interpolator = NearestNeighborInterpolator()
  )

  val refSSM = StatisticalMeshModel(refMesh, lowRankGP)


  // define attribute correspondences to attribute correspondences

  def attributeCorrespondence(movingMesh: TriangleMesh[_3D], ptIds : Seq[PointId]) : Seq[(PointId, Point[_3D])] = {
    ptIds.map{ id: PointId =>
      val pt = movingMesh.pointSet.point(id)
      val closestPointOnTarget = targetMesh.pointSet.findClosestPoint(pt).point
      (id, closestPointOnTarget)
    }
  }

  // compute GP regression as to 'fit model'

  val littleNoise = MultivariateNormalDistribution(DenseVector.zeros[Double](3), DenseMatrix.eye[Double](3))

  def fitModel(correspondences : Seq[(PointId, Point[_3D])]) : TriangleMesh[_3D] = {
    val regressionData = correspondences.map(correspondence =>
      (correspondence._1, correspondence._2, littleNoise)
    )
    val posterior = refSSM.posterior(regressionData.toIndexedSeq)
    posterior.mean
  }


  // non rigid ICP

  def nonRigidICP(movingMesh: TriangleMesh[_3D], ptIds : Seq[PointId], numberOfIterations : Int) : TriangleMesh[_3D] = {
    if (numberOfIterations == 0)
      movingMesh
    else{
      val correspondences = attributeCorrespondence(movingMesh, ptIds)
      val transformed = fitModel(correspondences)

      nonRigidICP(transformed, ptIds, numberOfIterations - 1)
    }
  }

  val penultimateFit = nonRigidICP(refSSM.mean, refIndexIds, 80)
  val penultimateFitCloud = penultimateFit.pointSet
  val penultimateFitCloudShow = ui.show(group2, penultimateFitCloud, "almostFit")
  penultimateFitCloudShow.color = java.awt.Color.CYAN
  penultimateFitCloudShow.radius = 0.8




// reg eval metrics

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

//    evaluating registration

  val avgDist = averageDistance(targetMesh,penultimateFit)
  println(avgDist)
  val hausDist = hausDistance(targetMesh,penultimateFit)
  println(hausDist)

//  final registration step

  def finalAttribute (targetMesh: TriangleMesh[_3D], ptIds : Seq[PointId]) : Seq[Point[_3D]] = {
     ptIds.map{ id : PointId =>
       val pt = penultimateFitCloud.point(id)
       val closesPointOnTarget = targetMesh.pointSet.findClosestPoint(pt).point
       (closesPointOnTarget)
     }
   }


  val finalRegisteredPoints = finalAttribute(targetMesh, refIndexIds)
  val finalRegisteredCloud = finalRegisteredPoints.toIndexedSeq
  val finalRegisteredMesh : TriangleMesh[_3D] = TriangleMesh3D(finalRegisteredCloud,TriangleList(IndexedSeq()))


  val group3 = ui.createGroup("Final Result")
  val finalRegisteredView = ui.show(group3, finalRegisteredCloud, "Final Registered Cloud")
  finalRegisteredView.color = java.awt.Color.GREEN
  finalRegisteredView.radius = 0.8


// uncomment bottom section to write to file

//  val newCloud = for((point,id) <- finalRegisteredCloud.zipWithIndex) yield {
//    Landmark(s"$id", point)
//  }
//  LandmarkIO.writeLandmarksCsv(newCloud, new java.io.File("finalRegisterResults/" + target))


}



