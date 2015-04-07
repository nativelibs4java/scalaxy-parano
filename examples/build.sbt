scalaVersion := "2.11.6"

autoCompilerPlugins := true

addCompilerPlugin("com.nativelibs4java" %% "scalaxy-parano" % "0.4-SNAPSHOT")

// Ensure Scalaxy/Parano's plugin is used.
scalacOptions += "-Xplugin-require:scalaxy-parano"

// Scalaxy/Parano snapshots are published on the Sonatype repository.
resolvers += Resolver.sonatypeRepo("snapshots")
