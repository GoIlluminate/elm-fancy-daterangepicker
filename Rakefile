
task :default => [:npm, :test, :watch]
task :jenkins => [:npm, :test, :build]

task :npm do
    sh("npm install")
    sh("cd example && npm install")
end

task :watch do
    sh("cd example && npm run start")
end

task :build do
    sh("cd example && npm run build")
end

task :test do
    sh("./node_modules/.bin/elm-test --compiler=./node_modules/.bin/elm")
end