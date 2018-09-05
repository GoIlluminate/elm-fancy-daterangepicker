
task :default => [:npm, :test, :watch]
task :jenkins => [:npm, :test, :build]

task :npm do
    sh("cd example && npm install")
end

task :watch do
    sh("cd example && npm run start")
end

task :build do
    sh("cd example && npm run build")
end

task :test do
    sh("elm-test")
end