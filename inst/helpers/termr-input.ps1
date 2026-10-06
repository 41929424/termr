# termr input helper for Windows consoles.
#
# R cannot read single key presses or mouse events from a Windows console
# without native code, so termr starts this script as a child process that
# shares R's console. It reads console input records and console size
# changes and writes them to stdout (a pipe read by R) as tab separated
# lines of ASCII numbers:
#
#   S <width> <height>                          console size (first line = ready)
#   K <virtual key> <char> <mods>               key press; mods: 1 Alt, 2 Shift, 4 Ctrl
#   M <x> <y> <buttons> <flags> <mods>          mouse record (1-based position)
#   E <message>                                 fatal error
#
# The script switches the console input to "raw" mode (no line editing,
# no echo, Ctrl+C as a key, mouse input on, quick-edit off), enables
# virtual terminal (ANSI) processing on the output when needed, and
# restores both when it exits: when R creates the stop file, or when the R
# process disappears. If the console API is not available it falls back to
# Console.ReadKey (keys only).

param(
  [Parameter(Mandatory = $true)][string]$StopFile,
  [Parameter(Mandatory = $true)][int]$ParentPid,
  [int]$Mouse = 1
)

$ErrorActionPreference = 'Stop'
$out = [Console]::Out

function Send([string]$line) {
  $out.WriteLine($line)
  $out.Flush()
}

function Mods([uint32]$state) {
  $m = 0
  if (($state -band 0x3) -ne 0) { $m = $m -bor 1 }   # right/left alt
  if (($state -band 0x10) -ne 0) { $m = $m -bor 2 }  # shift
  if (($state -band 0xC) -ne 0) { $m = $m -bor 4 }   # right/left ctrl
  $m
}

$native = $false
$inputMode = $null
$vt = $null
$restoreCtrlC = $null
try {
  try {
    Add-Type -Namespace TermrInput -Name Con -MemberDefinition @'
[StructLayout(LayoutKind.Explicit, CharSet = CharSet.Unicode)]
public struct Rec {
  [FieldOffset(0)] public ushort EventType;
  [FieldOffset(4)] public int KeyDown;
  [FieldOffset(8)] public ushort Repeat;
  [FieldOffset(10)] public ushort VirtualKey;
  [FieldOffset(12)] public ushort ScanCode;
  [FieldOffset(14)] public char Char;
  [FieldOffset(16)] public uint KeyState;
  [FieldOffset(4)] public short MouseX;
  [FieldOffset(6)] public short MouseY;
  [FieldOffset(8)] public uint Buttons;
  [FieldOffset(12)] public uint MouseState;
  [FieldOffset(16)] public uint MouseFlags;
}
[DllImport("kernel32.dll", SetLastError = true)]
public static extern System.IntPtr GetStdHandle(int n);
[DllImport("kernel32.dll", SetLastError = true, CharSet = CharSet.Unicode)]
public static extern System.IntPtr CreateFileW(string name, uint access, uint share,
  System.IntPtr security, uint disposition, uint flags, System.IntPtr template);
[DllImport("kernel32.dll", SetLastError = true)]
public static extern bool GetConsoleMode(System.IntPtr handle, out uint mode);
[DllImport("kernel32.dll", SetLastError = true)]
public static extern bool SetConsoleMode(System.IntPtr handle, uint mode);
[DllImport("kernel32.dll", SetLastError = true)]
public static extern bool GetNumberOfConsoleInputEvents(System.IntPtr handle, out uint n);
[DllImport("kernel32.dll", SetLastError = true, CharSet = CharSet.Unicode)]
public static extern bool ReadConsoleInputW(System.IntPtr handle, [Out] Rec[] buffer, uint length, out uint read);
'@
    $native = $true
  } catch {
    $native = $false
  }

  if ($native) {
    $inHandle = [TermrInput.Con]::GetStdHandle(-10)
    $mode = [uint32]0
    if ([TermrInput.Con]::GetConsoleMode($inHandle, [ref]$mode)) {
      # + EXTENDED_FLAGS (+ MOUSE_INPUT); - PROCESSED, LINE, ECHO, QUICK_EDIT
      $new = ($mode -bor 0x80) -band (-bnot 0x47)
      if ($Mouse -ne 0) { $new = $new -bor 0x10 }
      if ([TermrInput.Con]::SetConsoleMode($inHandle, $new)) {
        $inputMode = @{ Handle = $inHandle; Mode = $mode }
      }
    } else {
      $native = $false
    }
    if (-not $env:WT_SESSION -and -not $env:TERMR_SKIP_VT) {
      try {
        # GENERIC_READ | GENERIC_WRITE (0xC0000000; written as a decimal
        # UInt32 because PowerShell reads the hex literal as a negative
        # Int32), FILE_SHARE_READ | FILE_SHARE_WRITE, OPEN_EXISTING
        $access = [uint32]3221225472
        $outHandle = [TermrInput.Con]::CreateFileW('CONOUT$', $access, 3, [IntPtr]::Zero, 3, 0, [IntPtr]::Zero)
        $omode = [uint32]0
        if ([TermrInput.Con]::GetConsoleMode($outHandle, [ref]$omode) -and (($omode -band 4) -eq 0)) {
          # ENABLE_VIRTUAL_TERMINAL_PROCESSING
          [void][TermrInput.Con]::SetConsoleMode($outHandle, $omode -bor 4)
          $vt = @{ Handle = $outHandle; Mode = $omode }
        }
      } catch {
        # VT processing could not be enabled; output may show raw escapes.
      }
    }
  }
  if (-not $native) {
    try {
      $restoreCtrlC = [Console]::TreatControlCAsInput
      [Console]::TreatControlCAsInput = $true
    } catch {
      $restoreCtrlC = $null
    }
  }

  $raw = $Host.UI.RawUI
  $size = $raw.WindowSize
  $width = $size.Width
  $height = $size.Height
  Send ("S`t{0}`t{1}" -f $width, $height)

  $buffer = $null
  if ($native) { $buffer = New-Object 'TermrInput.Con+Rec[]' 64 }
  $tick = 0
  while ($true) {
    $got = 0
    if ($native) {
      $n = [uint32]0
      [void][TermrInput.Con]::GetNumberOfConsoleInputEvents($inHandle, [ref]$n)
      if ($n -gt 0) {
        $read = [uint32]0
        [void][TermrInput.Con]::ReadConsoleInputW($inHandle, $buffer, 64, [ref]$read)
        $top = $raw.WindowPosition.Y
        for ($i = 0; $i -lt $read; $i++) {
          $r = $buffer[$i]
          if ($r.EventType -eq 1 -and $r.KeyDown -ne 0) {
            $line = "K`t{0}`t{1}`t{2}" -f $r.VirtualKey, [int]$r.Char, (Mods $r.KeyState)
            for ($j = 0; $j -lt [Math]::Max(1, $r.Repeat); $j++) { Send $line }
          } elseif ($r.EventType -eq 2) {
            Send ("M`t{0}`t{1}`t{2}`t{3}`t{4}" -f ($r.MouseX + 1), ($r.MouseY - $top + 1), $r.Buttons, $r.MouseFlags, (Mods $r.MouseState))
          }
        }
        $got = $read
      }
    } else {
      while ([Console]::KeyAvailable -and $got -lt 64) {
        $k = [Console]::ReadKey($true)
        Send ("K`t{0}`t{1}`t{2}" -f [int]$k.Key, [int]$k.KeyChar, [int]$k.Modifiers)
        $got++
      }
    }
    if ($got -gt 0) { continue }

    $tick++
    if ($tick % 4 -eq 0) {
      $size = $raw.WindowSize
      if ($size.Width -ne $width -or $size.Height -ne $height) {
        $width = $size.Width
        $height = $size.Height
        Send ("S`t{0}`t{1}" -f $width, $height)
      }
      if (Test-Path -LiteralPath $StopFile) { break }
    }
    if ($tick % 60 -eq 0) {
      if (-not (Get-Process -Id $ParentPid -ErrorAction SilentlyContinue)) { break }
    }
    Start-Sleep -Milliseconds 15
  }
} catch {
  try { Send ("E`t" + ($_.Exception.Message -replace "[`r`n`t]", ' ')) } catch {}
  exit 2
} finally {
  if ($null -ne $inputMode) {
    try { [void][TermrInput.Con]::SetConsoleMode($inputMode.Handle, $inputMode.Mode) } catch {}
  }
  if ($null -ne $restoreCtrlC) {
    try { [Console]::TreatControlCAsInput = $restoreCtrlC } catch {}
  }
  if ($null -ne $vt) {
    try { [void][TermrInput.Con]::SetConsoleMode($vt.Handle, $vt.Mode) } catch {}
  }
}
