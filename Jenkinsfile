#!/usr/bin/env groovy

node('linux') {
    final BUILD_INCREMENT = 0

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
        if (env.BRANCH_NAME != "master") {
            env.VERSION = 0
        } else {
            env.VERSION = BUILD_NUMBER.toInteger() + BUILD_INCREMENT
        }

        // Print environment for debugging purposes
        sh "printenv"

        // Increment version in package.yaml prior to build
        updatedElmJson = incrementVersionInElmJson(readFile('elm.json'))
        writeFile file: 'elm.json', text: updatedElmJson

        // Run the rake build
        sh "rake jenkins --trace"
    }

    // We only want to tag git commits for "master" builds since they will have a legit build #.
      if (env.BRANCH_NAME == 'master') {
         stage('Tag Build') {
            withCredentials([[$class: 'UsernamePasswordMultiBinding', credentialsId: 'github-user',
                          usernameVariable: 'USERNAME', passwordVariable: 'PASSWORD']]) {
               
               def versionStr = getVersionFromElmJson(readFile('elm.json'))
               println versionStr

               def gitUrl = sh (
                  script: "git config --get remote.origin.url",
                  returnStdout: true
               ).trim()
               def authUrl = getAuthenticatedGitUrl(gitUrl, USERNAME, PASSWORD)

               // "git tag" will create a tag and a corresponding release on github, so we
               // can quickly reference the exact commit that was used to build any given version.
               sh "git tag -a $versionStr -m '[Jenkins] tagging build.'"
               sh "git push $authUrl --tags"
            }
         }
      }

}

def getVersionFromElmJson(elmJson) {
   def versionMatcher = elmJson =~ /(?m)"version":\s"((\d+.\d+).\d+)"/
   if(versionMatcher) {
      def versionPrefix = versionMatcher[0][2]
      return versionPrefix + "." + env.VERSION
   } else {
      error('Failed to find version string in elm.json')
   }
}

def incrementVersionInElmJson(elmJson) {
    def versionStr = getVersionFromElmJson(elmJson)
    elmJson.replaceFirst(/(?m)"version":\s"(\d+.\d+.\d+)"/, "\"version\": \"$versionStr\"")
}

// gitUrl is expected to be an HTTPS git URL without username/password paramters.
// We insert the username:password into the URL.
def getAuthenticatedGitUrl(gitUrl, USERNAME, PASSWORD) {
   def urlMatcher = gitUrl =~ /(?m)^https:\/\/(.*)/
   if (urlMatcher) {
      def urlSuffix = urlMatcher[0][1]

      return "https://$USERNAME:$PASSWORD@$urlSuffix"
   } else {
      error('Failed to match git URL')
   }
}

def getCommitHash() {
   return sh (
      script: "git rev-parse HEAD",
      returnStdout: true
   ).substring(0,8)
}