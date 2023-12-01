plugins {
    kotlin("jvm") version "1.9.21"
    application
}

group = "de.jfriemel.aoc"
version = "1.0-SNAPSHOT"

repositories {
    mavenCentral()
}

dependencies {
    testImplementation(kotlin("test"))
}

tasks.test {
    useJUnitPlatform()
}

kotlin {
    jvmToolchain(21)
}

application {
    mainClass.set("de.jfriemel.aoc.MainKt")
}
