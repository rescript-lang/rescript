# Stops runaway test processes before they starve the CI runner. Hosted
# Windows runners that run out of memory lose contact with GitHub and leave no
# log at all, so report the largest processes and stop the test processes
# while the runner can still upload the log.
#
# Runs until the file named by WATCHDOG_RUNNING is removed. Writes its report
# to the file named by WATCHDOG_REPORT.

$names = 'rescript', 'rescript-rust', 'bsc'
$perProcessLimit = 3GB
$countLimit = 64
$minimumFree = 1.5GB

function Describe-Processes($processes) {
  $processes |
    Sort-Object WorkingSet64 -Descending |
    Select-Object -First 15 |
    ForEach-Object {
      $process = Get-CimInstance Win32_Process -Filter "ProcessId=$($_.Id)" -ErrorAction SilentlyContinue
      '{0,7} parent {1,7} {2,8:N0} MB {3,9:N1} s CPU  {4}' -f `
        $_.Id, $process.ParentProcessId, ($_.WorkingSet64 / 1MB), $_.CPU, `
        $(if ($process.CommandLine) { $process.CommandLine } else { $_.ProcessName })
    }
}

while (Test-Path -LiteralPath $env:WATCHDOG_RUNNING) {
  Start-Sleep -Seconds 5
  $tested = @(Get-Process -Name $names -ErrorAction SilentlyContinue)
  if ($tested.Count -eq 0) { continue }
  $free = (Get-CimInstance Win32_OperatingSystem).FreePhysicalMemory * 1KB
  $reason = $null
  $large = @($tested | Where-Object { $_.WorkingSet64 -gt $perProcessLimit })
  if ($large.Count -gt 0) {
    $reason = "process $($large[0].Id) uses $([int]($large[0].WorkingSet64 / 1MB)) MB"
  } elseif ($tested.Count -gt $countLimit) {
    $reason = "$($tested.Count) rescript/bsc processes are running"
  } elseif ($free -lt $minimumFree) {
    $reason = "free memory is down to $([int]($free / 1MB)) MB"
  }
  if ($reason) {
    $report = @("Process watchdog: $reason; stopping rescript/bsc processes.") +
      'Largest processes:' + (Describe-Processes (Get-Process)) +
      'Stopped test processes:' + (Describe-Processes $tested)
    $report | Add-Content -LiteralPath $env:WATCHDOG_REPORT
    $report | Write-Host
    $tested | Stop-Process -Force -ErrorAction SilentlyContinue
  }
}
