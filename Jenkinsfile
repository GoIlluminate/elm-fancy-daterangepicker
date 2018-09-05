#!/usr/bin/env groovy

node('linux') {

    stage('Clean Workspace') {
      step([$class: 'WsCleanup'])
   }

   stage('Checkout') {
      // Checkout code from repository
      checkout scm
   }

   stage('Build and Test') {
        // Set msbuild log level:
        // Optional levels: quiet, minimal, normal, detailed, and diagnostic
        env.verbosity="normal"

        // Run the rake build
        sh "rake jenkins --trace"
    }
    
}