val avroVersion = "1.12.2"
val catsVersion = "2.13.0"
val circeVersion = "0.14.17"
val disciplineScalaTestVersion = "2.3.0"
val enumeratumVersion = "1.9.8"
val magnolia2Version = "1.1.14"
val magnolia3Version = "1.3.24"
val munitScalaCheckVersion = "1.3.1"
val munitVersion = "1.3.6"
val refinedVersion = "0.11.4"
val scala213Version = "2.13.18"
val scala3Version = "3.3.9"
val scalaJavaTimeVersion = "2.9.0"
val scodecBitsVersion = "1.2.5"
val slf4jNopVersion = "2.0.20"
val vulcan1xVersion = "1.14.1"

inThisBuild(
  Seq(
    crossScalaVersions := Seq(scala213Version, scala3Version),
    developers := List(tlGitHubDev("vlovgr", "Viktor Rudebeck")),
    githubWorkflowJavaVersions := Seq(JavaSpec.temurin("21")),
    organization := "org.typelevel",
    scalaVersion := scala3Version,
    semanticdbEnabled := true,
    semanticdbVersion := scalafixSemanticdb.revision,
    startYear := Some(2019),
    tlBaseVersion := "2.0",
    tlFatalWarnings := false,
    tlSiteKeepFiles := false,
    // The website is published from series/1.x until the 2.0.0 release
    tlSitePublishBranch := None,
    tlSitePublishTags := false
  )
)

lazy val root = tlCrossRootProject
  .aggregate(benchmarks, benchmarks1x, core, enumeratum, generic, refined, tests, unidocs)

lazy val benchmarks = project
  .in(file("modules/benchmarks"))
  .enablePlugins(JmhPlugin, NoPublishPlugin)
  .dependsOn(core.jvm)
  .settings(
    name := "vulcan-benchmarks",
    libraryDependencies ++= Seq(
      "org.apache.avro" % "avro" % avroVersion,
      "org.slf4j" % "slf4j-nop" % slf4jNopVersion
    ),
    javacOptions += "-Xlint:-options",
    headerSettings
  )

lazy val benchmarks1x = project
  .in(file("modules/benchmarks-1x"))
  .enablePlugins(JmhPlugin, NoPublishPlugin)
  .settings(
    name := "vulcan-benchmarks-1x",
    libraryDependencies ++= Seq(
      "com.github.fd4s" %% "vulcan" % vulcan1xVersion,
      "org.slf4j" % "slf4j-nop" % slf4jNopVersion
    ),
    javacOptions += "-Xlint:-options",
    headerSettings
  )

lazy val core = crossProject(JVMPlatform, JSPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .in(file("modules/core"))
  .settings(
    name := "vulcan",
    libraryDependencies ++= Seq(
      "io.circe" %%% "circe-core" % circeVersion,
      "io.circe" %%% "circe-jawn" % circeVersion,
      "org.scodec" %%% "scodec-bits" % scodecBitsVersion,
      "org.typelevel" %%% "cats-free" % catsVersion
    ),
    headerSettings
  )

lazy val enumeratum = crossProject(JVMPlatform, JSPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .in(file("modules/enumeratum"))
  .dependsOn(core, generic)
  .settings(
    name := "vulcan-enumeratum",
    libraryDependencies ++= Seq(
      "com.beachape" %%% "enumeratum" % enumeratumVersion
    ),
    headerSettings
  )

lazy val generic = crossProject(JVMPlatform, JSPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .in(file("modules/generic"))
  .dependsOn(core)
  .settings(
    name := "vulcan-generic",
    libraryDependencies ++= Seq(
      "org.typelevel" %%% "cats-core" % catsVersion,
      "org.typelevel" %%% "cats-free" % catsVersion
    ) ++ {
      if (tlIsScala3.value)
        Seq("com.softwaremill.magnolia1_3" %%% "magnolia" % magnolia3Version)
      else
        Seq("com.softwaremill.magnolia1_2" %%% "magnolia" % magnolia2Version)
    } ++ scalaReflect(scalaVersion.value),
    headerSettings
  )

lazy val refined = crossProject(JVMPlatform, JSPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .in(file("modules/refined"))
  .dependsOn(core)
  .settings(
    name := "vulcan-refined",
    libraryDependencies ++= Seq(
      "eu.timepit" %%% "refined" % refinedVersion,
      "org.typelevel" %%% "cats-core" % catsVersion
    ),
    headerSettings
  )

lazy val docs = project
  .in(file("site"))
  .enablePlugins(TypelevelSitePlugin)
  .dependsOn(core.jvm, enumeratum.jvm, generic.jvm, refined.jvm)
  .settings {
    import laika.ast.Image
    import laika.ast.InlineSVGIcon
    import laika.ast.LengthUnit.em
    import laika.ast.LengthUnit.px
    import laika.ast.Path.Root
    import laika.ast.Styles
    import laika.config.ApiLinks
    import laika.config.LinkConfig
    import laika.helium.Helium
    import laika.helium.config.ButtonLink
    import laika.helium.config.ColorQuintet
    import laika.helium.config.Favicon
    import laika.helium.config.HeliumIcon
    import laika.helium.config.IconLink
    import laika.helium.config.LinkGroup
    import laika.helium.config.LinkPanel
    import laika.helium.config.ReleaseInfo
    import laika.helium.config.TextLink
    import laika.theme.config.Color
    import org.typelevel.sbt.site.GenericSiteSettings

    Seq(
      crossScalaVersions := Seq(scala3Version),
      scalacOptions ~= (_.filterNot(_.startsWith("-Wunused"))),
      scalacOptions += "-Yretain-trees",
      Compile / packageDoc / mappings := (unidocs / Compile / packageDoc / mappings).value.map {
        case (file, path) => file -> path.replace('\\', '/')
      },
      laikaConfig ~= (_.withConfigValue(LinkConfig.empty.addApiLinks(ApiLinks("/api/")))),
      laikaIncludeAPI := true,
      tlSiteApiUrl := None,
      tlSitePreview := tlSitePreview.dependsOn(laikaGenerateAPI).value,
      mdocVariables ++= Map(
        "CATS_VERSION" -> catsVersion,
        "CIRCE_VERSION" -> circeVersion,
        "CORE_CROSS_SCALA_VERSIONS" -> crossScalaVersionsOf(core.jvm).value,
        "CORE_MODULE_NAME" -> (core.jvm / moduleName).value,
        "ENUMERATUM_CROSS_SCALA_VERSIONS" -> crossScalaVersionsOf(enumeratum.jvm).value,
        "ENUMERATUM_MODULE_NAME" -> (enumeratum.jvm / moduleName).value,
        "ENUMERATUM_VERSION" -> enumeratumVersion,
        "GENERIC_CROSS_SCALA_VERSIONS" -> crossScalaVersionsOf(generic.jvm).value,
        "GENERIC_MODULE_NAME" -> (generic.jvm / moduleName).value,
        "MAGNOLIA_VERSION" -> (if (tlIsScala3.value) magnolia3Version else magnolia2Version),
        "MAJOR_VERSION" -> majorVersion(version.value),
        "ORGANIZATION" -> (ThisBuild / organization).value,
        "REFINED_CROSS_SCALA_VERSIONS" -> crossScalaVersionsOf(refined.jvm).value,
        "REFINED_MODULE_NAME" -> (refined.jvm / moduleName).value,
        "REFINED_VERSION" -> refinedVersion,
        "SCALA_DOCS_VERSION" -> scalaVersionOf(scalaVersion.value),
        "SCALA_JAVA_TIME_VERSION" -> scalaJavaTimeVersion,
        "SCALA_JS_MAJOR_MINOR_VERSION" -> scalaJsMajorMinorVersion,
        "SCALA_NATIVE_MAJOR_MINOR_VERSION" -> scalaNativeMajorMinorVersion,
        "SCALA_PUBLISH_VERSIONS" -> crossScalaVersionsOf(core.jvm).value,
        "SCODEC_BITS_VERSION" -> scodecBitsVersion
      ),
      tlSiteHelium := {
        val gitHubUrl = "https://github.com/typelevel/vulcan"

        val projectLinks = List(
          IconLink.internal(Root / "api" / "vulcan.html", HeliumIcon.api),
          IconLink.external(gitHubUrl, HeliumIcon.github),
          IconLink.external("https://discord.gg/Q8KBnyC776", HeliumIcon.chat)
        )

        Helium.defaults
          .extendWith(GenericSiteSettings.themeExtensions.value)
          .site
          .metadata(
            title = Some("Vulcan · Functional Avro for Scala"),
            description = Some("Functional Avro encodings for Scala on the JVM, Scala.js, and Scala Native."),
            authors = developers.value.map(_.name),
            language = Some("en"),
            version = Some(version.value)
          )
          .site
          .themeColors(
            primary = Color.hex("2a1b12"),
            secondary = Color.hex("985516"),
            primaryMedium = Color.hex("ebdecb"),
            primaryLight = Color.hex("f8f6f1"),
            text = Color.hex("24292e"),
            background = Color.hex("ffffff"),
            bgGradient = (Color.hex("2a1b12"), Color.hex("3a2716"))
          )
          .site
          .syntaxHighlightingColors(
            base = ColorQuintet(
              Color.hex("f8f8f8"), // background
              Color.hex("999988"), // comments
              Color.hex("333333"), // punctuation
              Color.hex("333333"), // identifiers
              Color.hex("333333") // unclassified
            ),
            wheel = ColorQuintet(
              Color.hex("999999"), // substitutions, annotations
              Color.hex("333333"), // keywords, escape sequences
              Color.hex("990000"), // declaration names
              Color.hex("dd1144"), // literals
              Color.hex("445588") // types
            )
          )
          .site
          .clearFontResources
          .site
          .fontSizes(
            body = px(16),
            code = em(0.85),
            title = px(32),
            header2 = px(24),
            header3 = px(20),
            header4 = px(16),
            small = px(12)
          )
          .site
          .darkMode
          .disabled
          .site
          .layout(topBarHeight = px(50))
          .site
          .topNavigationBar(
            homeLink = IconLink.internal(
              Root / "README",
              InlineSVGIcon(IO.read(file("docs/img/logo.svg"))),
              text = Some("Vulcan"),
              options = Styles("home-link")
            ),
            navLinks = projectLinks,
            highContrast = true
          )
          .site
          .internalCSS(Root / "css")
          .site
          .favIcons(
            Favicon.external("https://typelevel.org/vulcan/img/logo.png", "180x180", "image/png"),
            Favicon.internal(Root / "img" / "logo.svg")
          )
          .site
          .landingPage(
            logo = Some(
              Image.internal(Root / "img" / "logo.svg", height = Some(px(150)), alt = Some(""))
            ),
            title = Some("Vulcan"),
            subtitle = Some("Functional Avro for Scala"),
            latestReleases = tlLatestVersion.value.toList.map(ReleaseInfo("Latest Release", _)),
            license = Some("Apache 2.0"),
            titleLinks = Seq(
              ButtonLink.internal(Root / "overview.md", "Documentation"),
              LinkGroup.create(projectLinks.head, projectLinks.tail: _*)
            ),
            linkPanel = Some(
              LinkPanel(
                "Documentation",
                TextLink.internal(Root / "overview.md", "Overview"),
                TextLink.internal(Root / "codecs.md", "Codecs"),
                TextLink.internal(Root / "derivation.md", "Derivation"),
                TextLink.internal(Root / "integrations" / "enumeratum.md", "Integrations")
              )
            )
          )
          .site
          .footer()
          .site
          .pageNavigation(
            sourceBaseURL = Some("https://github.com/typelevel/vulcan/tree/main/docs")
          )
      }
    )
  }

lazy val tests = crossProject(JVMPlatform, JSPlatform, NativePlatform)
  .crossType(CrossType.Full)
  .in(file("tests"))
  .enablePlugins(NoPublishPlugin)
  .dependsOn(enumeratum, generic, refined)
  .settings(
    name := "vulcan-tests",
    libraryDependencies ++= Seq(
      "eu.timepit" %%% "refined-scalacheck" % refinedVersion % Test,
      "org.scalameta" %%% "munit" % munitVersion % Test,
      "org.scalameta" %%% "munit-scalacheck" % munitScalaCheckVersion % Test,
      "org.typelevel" %%% "cats-kernel-laws" % catsVersion % Test,
      "org.typelevel" %%% "cats-laws" % catsVersion % Test,
      "org.typelevel" %%% "discipline-scalatest" % disciplineScalaTestVersion % Test
    ),
    headerSettings,
    Test / scalacOptions ++= (if (tlIsScala3.value) Seq("-Yretain-trees") else Seq())
  )
  .platformsSettings(JSPlatform, NativePlatform)(
    libraryDependencies += "io.github.cquiroz" %%% "scala-java-time" % scalaJavaTimeVersion % Test
  )
  .jvmSettings(
    libraryDependencies ++= Seq(
      "org.apache.avro" % "avro" % avroVersion % Test,
      "org.slf4j" % "slf4j-nop" % slf4jNopVersion % Test
    )
  )

lazy val unidocs = project
  .enablePlugins(TypelevelUnidocPlugin)
  .settings(
    name := "vulcan-docs",
    headerSettings,
    crossScalaVersions := Seq(scala3Version),
    semanticdbEnabled := false,
    ScalaUnidoc / unidoc / unidocProjectFilter :=
      inProjects(core.jvm, enumeratum.jvm, generic.jvm, refined.jvm),
    ScalaUnidoc / unidoc / apiMappings := {
      val docsApiUrl = apiURL.value.map(_.toString)
      (ScalaUnidoc / unidoc / apiMappings).value.filterNot { case (_, url) =>
        docsApiUrl.contains(url.toString)
      }
    },
    ScalaUnidoc / unidoc / scalacOptions := {
      val docVersion = tlLatestVersion.value.getOrElse(version.value)
      val logo = (LocalRootProject / baseDirectory).value / "docs" / "img" / "logo.svg"
      val options = (ScalaUnidoc / unidoc / scalacOptions).value.filter {
        case "-Werror" | "-Wvalue-discard" => false
        case option if option.startsWith("-Ykind-projector") => false
        case _ => true
      }
      val projectOptions = withOption(options, "-project", "Vulcan")
      withOption(projectOptions, "-project-version", s"v$docVersion") ++ Seq(
        "-comment-syntax:wiki",
        "-doc-canonical-base-url:https://typelevel.org/vulcan/api/",
        "-groups",
        "-project-footer:Licensed under Apache License, Version 2.0.",
        s"-project-logo:${logo.getAbsolutePath}",
        "-social-links:github::https://github.com/typelevel/vulcan",
        "-social-links:discord::https://discord.gg/Q8KBnyC776"
      )
    }
  )

lazy val headerSettings = Seq(
  headerLicense := Some(
    HeaderLicense.ALv2(
      startYear.value.get.toString,
      "OVO Energy Limited",
      HeaderLicenseStyle.SpdxSyntax
    )
  )
)

def scalaReflect(scalaVersion: String): Seq[ModuleID] =
  if (scalaVersion.startsWith("2."))
    Seq("org.scala-lang" % "scala-reflect" % scalaVersion % Provided)
  else Seq()

def majorVersion(version: String): String =
  version.split('.').head

def withOption(options: Seq[String], name: String, value: String): Seq[String] =
  options.indexOf(name) match {
    case -1 => options ++ Seq(name, value)
    case index => options.updated(index + 1, value)
  }

def scalaVersionOf(version: String): String = {
  if (version.contains("-")) version
  else {
    val (major, minor) =
      CrossVersion.partialVersion(version).get
    s"$major.$minor"
  }
}

def crossScalaVersionsOf(project: Project): Def.Initialize[String] =
  Def.setting {
    val scalaVersions = (project / crossScalaVersions).value.map(scalaVersionOf)
    if (scalaVersions.size <= 2) scalaVersions.mkString(" and ")
    else scalaVersions.init.mkString(", ") ++ " and " ++ scalaVersions.last
  }

lazy val scalaJsMajorMinorVersion =
  """"org.scala-js" % "sbt-scalajs" % "([^"]+)"""".r
    .findFirstMatchIn(IO.read(file("project/plugins.sbt")))
    .map(_.group(1))
    .flatMap(CrossVersion.partialVersion)
    .map { case (major, minor) => s"$major.$minor" }
    .getOrElse(throw new MessageOnlyException("unable to determine Scala.js plugin version"))

lazy val scalaNativeMajorMinorVersion =
  """"org.scala-native" % "sbt-scala-native" % "([^"]+)"""".r
    .findFirstMatchIn(IO.read(file("project/plugins.sbt")))
    .map(_.group(1))
    .flatMap(CrossVersion.partialVersion)
    .map { case (major, minor) => s"$major.$minor" }
    .getOrElse(throw new MessageOnlyException("unable to determine Scala Native plugin version"))
