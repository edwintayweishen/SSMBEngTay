object createPCA extends App{

  import scalismo.geometry._
  import scalismo.common._
  import scalismo.ui.api._
  import scalismo.io._
  import scalismo.statisticalmodel._


  scalismo.initialize()
  implicit val rng = scalismo.utils.Random(42)
  val ui = ScalismoUI()

  val landmarkFiles = new java.io.File("finalRegisterResults/").listFiles()
  val landmarksSequence : Array[Seq[Landmark[_3D]]] = landmarkFiles.map { f => LandmarkIO.readLandmarksCsv[_3D](f).get}

  type Landmarks = Seq[Landmark[_3D]]

  val landmarkSeq : Seq[Landmarks] = landmarksSequence

  val refLandmarks = LandmarkIO.readLandmarksCsv[_3D](new java.io.File("finalRegisterResults/604_3745_14_L_3D.obj")).get

  def landmarksToUnstructuredPointsDomain(landmarks: Landmarks) : UnstructuredPointsDomain[_3D] = {
    UnstructuredPointsDomain[_3D](landmarks.map(_.point).toIndexedSeq)
  }

   val refDomain = landmarksToUnstructuredPointsDomain(refLandmarks)

  val fields = for(landmarks <- landmarkSeq) yield{
    val domain = landmarksToUnstructuredPointsDomain(landmarks)

    def transformForPoint(pt: Point[_3D]): EuclideanVector[_3D] = {
      val id = domain.findClosestPoint(pt).id
      val refPoint = refDomain.point(id)
      pt - refPoint
    }
    Field(RealSpace[_3D], transformForPoint _)
  }

  val pcaModel = DiscreteLowRankGaussianProcess.createUsingPCA(refDomain,fields)

  val refMesh = MeshIO.readMesh(new java.io.File("feMeshes/604_3745_14_L_3D.stl")).get

  val testModel = StatisticalMeshModel.apply(refMesh,pcaModel)
  ui.show(testModel,"testModel")
  StatisticalModelIO.writeStatisticalMeshModel(testModel, new java.io.File("newModel.h5")).get


  // evaluate compactnesss

  val eigenvalues = testModel.gp.klBasis.map{k =>
    k.eigenvalue
  }

  val sum_eigenvalues = eigenvalues.sum

  print(eigenvalues.map(e => e/sum_eigenvalues))

//  val oldModel = StatismoIO.readStatismoMeshModel(new java.io.File("oldModel.h5")).get
//  ui.show(oldModel,"oldModel")

//  for (i <- 0 until 10) {
//
//    val sampleOfDeformationVectors = pcaModel.sample()
//
//    //ui.show(sampleOfDeformationVectors,s"sample-$i")
//
//    val sampleOfPoints = sampleOfDeformationVectors.pointsWithValues.toIndexedSeq.map{case (p,v) => p + v}
//    ui.show(sampleOfPoints, s"point-sample-$i")
//
//    val newCloud = for((point,id) <- sampleOfPoints.zipWithIndex) yield {
//      Landmark(s"$id", point)
//    }
//
//    LandmarkIO.writeLandmarksCsv(newCloud, new java.io.File("ssmSamples/" + s"point-sample-$i" + ".xyz"))
//
//  }




}
