
task :default => [:npm, :test, :watch]
task :jenkins => [:docker_build]

task :npm do
    sh("npm ci")
    sh("cd example && npm ci")
end

task :watch do
    sh("cd example && npm run start")
end

task :build do
    sh("cd example && npm run build")
end

task :css do 
    sh("rm -rf ./css/daterangepicker.css")
    sh("npm run scss")
end

task :test do
    sh("./node_modules/.bin/elm-test --compiler=./node_modules/.bin/elm")
end

desc "Builds Docker and also runs tests"
task :docker_build do
    ENV['DOCKER_BUILDKIT'] = '1'
    sh("docker build . -t elm-fancy-daterangepicker")
end