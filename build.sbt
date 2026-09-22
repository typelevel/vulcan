val avroVersion = "1.12.2"
val catsVersion = "2.13.0"
val disciplineScalaTestVersion = "2.3.0"
val enumeratumVersion = "1.9.8"
val jacksonVersion = "2.22.3"
val magnolia2Version = "0.17.0"
val magnolia3Version = "1.3.23"
val munitVersion = "1.3.6"
val munitScalaCheckVersion = "1.3.1"
val refinedVersion = "0.11.4"
val scalaCollectionCompatVersion = "2.14.0"
val scalacCompatVersion = "0.1.5"
val shapeless3Version = "3.6.0"
val shapelessVersion = "2.3.13"
val slf4jNopVersion = "2.0.19"

val scala212 = "2.12.21"
val scala213 = "2.13.18"
val scala3 = "3.3.8"

ThisBuild / tlBaseVersion := "1.14"

lazy val vulcan = project
  .in(file("."))
  .settings(
    scalaSettings,
    noPublishSettings,
    console := (core / Compile / console).value,
    Test / console := (core / Test / console).value
  )
  .enablePlugins(TypelevelMimaPlugin)
  .aggregate(core, enumeratum, generic, refined)

lazy val core = project
  .in(file("modules/core"))
  .settings(
    moduleName := "vulcan",
    name := moduleName.value,
    dependencySettings ++ Seq(
      libraryDependencies ++= Seq(
        "com.fasterxml.jackson.core" % "jackson-core" % jacksonVersion,
        "com.fasterxml.jackson.core" % "jackson-databind" % jacksonVersion,
        "org.apache.avro" % "avro" % avroVersion,
        "org.typelevel" %% "cats-free" % catsVersion,
        "org.typelevel" %% "scalac-compat-annotation" % scalacCompatVersion % Test
      ) ++ {
        if (scalaVersion.value.startsWith("3")) Nil
        else Seq("org.scala-lang" % "scala-reflect" % scalaVersion.value % Provided)
      }
    ),
    scalatestSettings,
    publishSettings,
    scalaSettings,
    testSettings,
    tlVersionIntroduced := Map("3" -> "1.7.0")
  )

lazy val enumeratum = project
  .in(file("modules/enumeratum"))
  .settings(
    moduleName := "vulcan-enumeratum",
    name := moduleName.value,
    dependencySettings ++ Seq(
      libraryDependencies ++= {
        if (scalaVersion.value.startsWith("2"))
          Seq(
            "com.beachape" %% "enumeratum" % enumeratumVersion,
            "org.apache.avro" % "avro" % avroVersion,
            "org.scala-lang" % "scala-reflect" % scalaVersion.value % Provided
          )
        else
          Seq(
            "com.beachape" %% "enumeratum" % enumeratumVersion,
            "org.apache.avro" % "avro" % avroVersion
          )
      }
    ),
    scalatestSettings,
    publishSettings,
    scalaSettings,
    testSettings,
    Test / scalacOptions ++= {
      if (scalaVersion.value.startsWith("3")) List("-Yretain-trees") else Nil
    },
    tlVersionIntroduced := Map("3" -> "1.14.0")
  )
  .dependsOn(core, generic)

lazy val generic = project
  .in(file("modules/generic"))
  .settings(
    moduleName := "vulcan-generic",
    name := moduleName.value,
    dependencySettings ++ Seq(
      libraryDependencies ++= {
        if (scalaVersion.value.startsWith("2"))
          Seq(
            "com.chuusai" %% "shapeless" % shapelessVersion,
            "com.propensive" %% "magnolia" % magnolia2Version,
            "org.apache.avro" % "avro" % avroVersion,
            "org.scala-lang" % "scala-reflect" % scalaVersion.value % Provided,
            "org.typelevel" %% "cats-core" % catsVersion,
            "org.typelevel" %% "cats-free" % catsVersion
          )
        else
          Seq(
            "com.softwaremill.magnolia1_3" %% "magnolia" % magnolia3Version,
            "org.apache.avro" % "avro" % avroVersion,
            "org.typelevel" %% "cats-core" % catsVersion,
            "org.typelevel" %% "cats-free" % catsVersion,
            "org.typelevel" %% "cats-kernel" % catsVersion,
            "org.typelevel" %% "shapeless3-deriving" % shapeless3Version
          )
      }
    ),
    scalatestSettings,
    publishSettings,
    scalaSettings,
    // magnolia requires compilation with the -Yretain-trees flag to support case class field default values on Scala 3
    Test / scalacOptions ++= (if (CrossVersion.partialVersion(scalaVersion.value).exists(_._1 == 3))
                                Seq("-Yretain-trees")
                              else Nil),
    testSettings,
    tlVersionIntroduced := Map("3" -> "1.8.0")
  )
  .dependsOn(core % "compile->compile;test->test")

lazy val refined = project
  .in(file("modules/refined"))
  .settings(
    moduleName := "vulcan-refined",
    name := moduleName.value,
    dependencySettings ++ Seq(
      libraryDependencies ++= Seq(
        "eu.timepit" %% "refined" % refinedVersion,
        "eu.timepit" %% "refined-scalacheck" % refinedVersion % Test,
        "org.typelevel" %% "cats-core" % catsVersion
      )
    ),
    munitSettings,
    publishSettings,
    scalaSettings,
    testSettings,
    tlVersionIntroduced := Map("3" -> "1.7.0")
  )
  .dependsOn(core)

lazy val docs = project
  .in(file("docs"))
  .settings(
    moduleName := "vulcan-docs",
    name := moduleName.value,
    dependencySettings,
    noPublishSettings,
    scalaSettings,
    mdocSettings,
    buildInfoSettings
  )
  .dependsOn(core, enumeratum, generic, refined)
  .enablePlugins(BuildInfoPlugin, DocusaurusPlugin, MdocPlugin, ScalaUnidocPlugin)

lazy val dependencySettings = Seq(
  libraryDependencies ++= {
    if (scalaVersion.value.startsWith("3")) Nil
    else {
      Seq(
        "org.scala-lang.modules" %% "scala-collection-compat" % scalaCollectionCompatVersion % Test,
        compilerPlugin(("org.typelevel" %% "kind-projector" % "0.13.4").cross(CrossVersion.full))
      )
    }
  },
  pomPostProcess := { (node: xml.Node) =>
    new xml.transform.RuleTransformer(new xml.transform.RewriteRule {
      def scopedDependency(e: xml.Elem): Boolean =
        e.label == "dependency" && e.child.exists(_.label == "scope")

      override def transform(node: xml.Node): xml.NodeSeq =
        node match {
          case e: xml.Elem if scopedDependency(e) => Nil
          case _                                  => Seq(node)
        }
    }).transform(node).head
  }
)

lazy val scalatestSettings = Seq(
  libraryDependencies ++= Seq(
    "org.typelevel" %% "discipline-scalatest" % disciplineScalaTestVersion,
    "org.typelevel" %% "cats-testkit" % catsVersion,
    "org.slf4j" % "slf4j-nop" % slf4jNopVersion
  ).map(_ % Test)
)

lazy val munitSettings = Seq(
  libraryDependencies ++= Seq(
    "org.scalameta" %% "munit" % munitVersion,
    "org.scalameta" %% "munit-scalacheck" % munitScalaCheckVersion,
    "org.slf4j" % "slf4j-nop" % slf4jNopVersion
  ).map(_ % Test),
  testFrameworks += new TestFramework("munit.Framework")
)

lazy val mdocSettings = Seq(
  mdoc := (Compile / run).evaluated,
  scalacOptions --= Seq("-Xfatal-warnings", "-Ywarn-unused"),
  crossScalaVersions := Seq(scala213),
  ScalaUnidoc / unidoc / unidocProjectFilter := inProjects(core, enumeratum, generic, refined),
  ScalaUnidoc / unidoc / target := (LocalRootProject / baseDirectory).value / "website" / "static" / "api",
  cleanFiles += (ScalaUnidoc / unidoc / target).value,
  docusaurusCreateSite := docusaurusCreateSite
    .dependsOn(Compile / unidoc)
    .dependsOn(ThisBuild / updateSiteVariables)
    .value,
  docusaurusPublishGhpages :=
    docusaurusPublishGhpages
      .dependsOn(Compile / unidoc)
      .dependsOn(ThisBuild / updateSiteVariables)
      .value,
  // format: off
  ScalaUnidoc / unidoc / scalacOptions ++= Seq(
    "-doc-source-url", s"https://github.com/typelevel/vulcan/tree/v${(ThisBuild / latestVersion).value}€{FILE_PATH}.scala",
    "-sourcepath", (LocalRootProject / baseDirectory).value.getAbsolutePath,
    "-doc-title", "Vulcan",
    "-doc-version", s"v${(ThisBuild / latestVersion).value}",
    "-groups"
  )
  // format: on
)

lazy val buildInfoSettings = Seq(
  buildInfoPackage := "vulcan.build",
  buildInfoObject := "info",
  buildInfoKeys := {
    val magnolia: String =
      if (scalaVersion.value.startsWith("3")) magnolia3Version else magnolia2Version
    Seq[BuildInfoKey](
      scalaVersion,
      scalacOptions,
      sourceDirectory,
      ThisBuild / latestVersion,
      BuildInfoKey.map(ThisBuild / version) { case (_, v) =>
        "latestSnapshotVersion" -> v
      },
      BuildInfoKey.map(core / moduleName) { case (k, v) =>
        "core" ++ k.capitalize -> v
      },
      BuildInfoKey.map(core / crossScalaVersions) { case (k, v) =>
        "core" ++ k.capitalize -> v
      },
      BuildInfoKey.map(enumeratum / moduleName) { case (k, v) =>
        "enumeratum" ++ k.capitalize -> v
      },
      BuildInfoKey.map(enumeratum / crossScalaVersions) { case (k, v) =>
        "enumeratum" ++ k.capitalize -> v
      },
      BuildInfoKey.map(generic / moduleName) { case (k, v) =>
        "generic" ++ k.capitalize -> v
      },
      BuildInfoKey.map(generic / crossScalaVersions) { case (k, v) =>
        "generic" ++ k.capitalize -> v
      },
      BuildInfoKey.map(refined / moduleName) { case (k, v) =>
        "refined" ++ k.capitalize -> v
      },
      BuildInfoKey.map(refined / crossScalaVersions) { case (k, v) =>
        "refined" ++ k.capitalize -> v
      },
      LocalRootProject / organization,
      core / crossScalaVersions,
      BuildInfoKey("avroVersion" -> avroVersion),
      BuildInfoKey("catsVersion" -> catsVersion),
      BuildInfoKey("enumeratumVersion" -> enumeratumVersion),
      BuildInfoKey("magnoliaVersion" -> magnolia),
      BuildInfoKey("refinedVersion" -> refinedVersion),
      BuildInfoKey("shapelessVersion" -> shapelessVersion)
    )
  }
)

lazy val metadataSettings = Seq(
  organization := "com.github.fd4s"
)

ThisBuild / githubWorkflowBuild := Seq(
  WorkflowStep.Sbt(List("ci")),
  WorkflowStep.Sbt(
    List("docs/run"),
    cond = Some(s"matrix.scala == '2.13'")
  )
)

ThisBuild / githubWorkflowArtifactUpload := false

ThisBuild / githubWorkflowJavaVersions := Seq(JavaSpec.temurin("21"))

ThisBuild / githubWorkflowPublish := Seq(
  WorkflowStep.Sbt(
    List("tlCiRelease", "docs/docusaurusPublishGhpages"),
    env = Map(
      "GIT_DEPLOY_KEY" -> "${{ secrets.GIT_DEPLOY_KEY }}",
      "PGP_PASSPHRASE" -> "${{ secrets.PGP_PASSPHRASE }}",
      "PGP_SECRET" -> "${{ secrets.PGP_SECRET }}",
      "SONATYPE_PASSWORD" -> "${{ secrets.SONATYPE_PASSWORD }}",
      "SONATYPE_USERNAME" -> "${{ secrets.SONATYPE_USERNAME }}"
    )
  )
)

lazy val publishSettings =
  metadataSettings ++ Seq(
    Test / publishArtifact := false,
    pomIncludeRepository := (_ => false),
    homepage := Some(url("https://typelevel.org/vulcan")),
    licenses := List("Apache-2.0" -> url("https://www.apache.org/licenses/LICENSE-2.0.txt")),
    startYear := Some(2019),
    headerLicense := Some(
      de.heikoseeberger.sbtheader.License.ALv2(
        s"${startYear.value.get}",
        "OVO Energy Limited",
        HeaderLicenseStyle.SpdxSyntax
      )
    ),
    headerSources / excludeFilter := HiddenFileFilter,
    developers := List(tlGitHubDev("vlovgr", "Viktor Rudebeck"))
  )

ThisBuild / mimaBinaryIssueFilters ++= {
  import com.typesafe.tools.mima.core.*
  // format: off
  Seq(
    ProblemFilters.exclude[Problem]("vulcan.internal.*"),
    ProblemFilters.exclude[IncompatibleSignatureProblem]("*"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("vulcan.Codec.withDecodingTypeName"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("vulcan.AvroError.decode*"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("vulcan.AvroError.encode*"),
    ProblemFilters.exclude[MissingClassProblem]("vulcan.Codec$Field$"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("vulcan.AvroException.*"),

    // package-private
    ProblemFilters.exclude[DirectMissingMethodProblem]("vulcan.Codec.instanceForTypes")
  )
  // format: on
}

lazy val noPublishSettings =
  publishSettings ++ Seq(
    publish / skip := true,
    publishArtifact := false
  )

ThisBuild / scalaVersion := scala213

ThisBuild / crossScalaVersions := Seq(scala212, scala213, scala3)

ThisBuild / tlFatalWarnings := false

lazy val scalaSettings = Seq(
  Compile / console / scalacOptions --= Seq("-Xlint", "-Ywarn-unused"),
  Test / console / scalacOptions := (Compile / console / scalacOptions).value,
  Compile / unmanagedSourceDirectories ++= {
    val sourceDir = (Compile / sourceDirectory).value
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, 12)) => Seq(sourceDir / "scala-2.12", sourceDir / "scala-2")
      case Some((2, 13)) => Seq(sourceDir / "scala-2.13+", sourceDir / "scala-2")
      case _             => Seq(sourceDir / "scala-2.13+", sourceDir / "scala-3")
    }
  },
  Test / unmanagedSourceDirectories ++= {
    val sourceDir = (Test / sourceDirectory).value
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, _)) => Seq(sourceDir / "scala-2")
      case _            => Nil
    }
  }
)

lazy val testSettings = Seq(
  Test / logBuffered := false,
  Test / parallelExecution := false,
  Test / testOptions += Tests.Argument("-oDF")
)

def scalaVersionOf(version: String): String = {
  if (version.contains("-")) version
  else {
    val (major, minor) =
      CrossVersion.partialVersion(version).get
    s"$major.$minor"
  }
}

val latestVersion = settingKey[String]("Latest stable released version")
ThisBuild / latestVersion := tlLatestVersion.value
  .getOrElse(
    throw new IllegalStateException("No tagged version found")
  )

val updateSiteVariables = taskKey[Unit]("Update site variables")
ThisBuild / updateSiteVariables := {
  val file =
    (LocalRootProject / baseDirectory).value / "website" / "variables.js"

  val variables =
    Map[String, String](
      "organization" -> (LocalRootProject / organization).value,
      "coreModuleName" -> (core / moduleName).value,
      "latestVersion" -> (ThisBuild / latestVersion).value,
      "scalaPublishVersions" -> {
        val scalaVersions = (core / crossScalaVersions).value.map(scalaVersionOf)
        if (scalaVersions.size <= 2) scalaVersions.mkString(" and ")
        else scalaVersions.init.mkString(", ") ++ " and " ++ scalaVersions.last
      }
    )

  val fileHeader =
    "// Generated by sbt. Do not edit directly."

  val fileContents =
    variables.toList
      .sortBy { case (key, _) => key }
      .map { case (key, value) => s"  $key: '$value'" }
      .mkString(s"$fileHeader\nmodule.exports = {\n", ",\n", "\n};\n")

  IO.write(file, fileContents)
}

def addCommandsAlias(name: String, values: List[String]) =
  addCommandAlias(name, values.mkString(";", ";", ""))

addCommandsAlias(
  "validate",
  List(
    "+clean",
    "+test",
    "+mimaReportBinaryIssues",
    "+scalafmtCheck",
    "scalafmtSbtCheck",
    "+headerCheck",
    "+doc",
    "docs/run"
  )
)

addCommandsAlias(
  "ci",
  List(
    "clean",
    "test",
    "mimaReportBinaryIssues",
    "scalafmtCheck",
    "scalafmtSbtCheck",
    "headerCheck",
    "doc"
  )
)
