
task :default => [:npm, :elm, :test, :watch]

task :npm do
    sh("cd example && npm install")
end

task :elm do
    sh("cd example && elm package install --yes")
end

task :watch do
    sh("cd example && npm run start")
end

task :test do
    sh("cd tests && elm package install --yes")
    sh("elm-test")
end