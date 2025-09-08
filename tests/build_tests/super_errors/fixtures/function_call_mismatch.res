@module("shelljs")
external cd: string => string = "cd"
external exec: string => string = "exec"

let cloneInTemp = (temp: string): string => {
  cd(temp)
  exec("git clone git@github.com:myorg/myrepo.git")
}
