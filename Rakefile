
task :default => [:npm, :elm, :watch]

task :npm do
    sh("cd example && npm install")
end

task :elm do
    sh("cd example && elm package install --yes")
end

task :watch do
    sh("cd example && npm run start")
end