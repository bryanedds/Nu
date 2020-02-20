$vsVersions="Visual Studio 2017", "Visual Studio 2019"
$myDocs=[environment]::getfolderpath("mydocuments")
$success=$false
foreach ($v in $vsVersions)
{
	$dest=Join-Path -Path $myDocs -ChildPath "$v\Templates\ProjectTemplates"
	if (Test-Path $dest -PathType Container) {
		Write-Host "Installing Nu.Game project template for $v..."
		Copy-Item Nu.Game.zip $dest
		$success=$true
	}
}
if ($success -eq $false) {
	Write-Host "Unable to find a supported Visual Studio directory in $myDocs"
}
Pause
