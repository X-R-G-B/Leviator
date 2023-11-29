$binStack = $args[0]
$binTarget = $args[1]

stack build

$pathLocalRoot = stack path --local-install-root

Copy-Item "$pathLocalRoot/bin/$binStack" "$binTarget"
