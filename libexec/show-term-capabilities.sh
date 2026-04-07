#!/usr/bin/env bash
# Test various terminal capabilities (underline, strikethrough, URL, etc.)

echo "Terminal Capabilities Test"
echo "=========================="
echo ""

# Basic text attributes
echo "Basic Text Attributes:"
echo "  Normal:    Normal text"
echo "  Bold:      $(printf '\033[1mBold text\033[0m')"
echo "  Dim:       $(printf '\033[2mDim text\033[0m')"
echo "  Italic:    $(printf '\033[3mItalic text\033[0m')"
echo "  Underline: $(printf '\033[4mUnderlined text\033[0m')"
echo "  Blink:     $(printf '\033[5mBlinking text\033[0m')"
echo "  Reverse:   $(printf '\033[7mReversed text\033[0m')"
echo "  Hidden:    $(printf '\033[8mHidden text\033[0m')"
echo "  Strike:    $(printf '\033[9mStrikethrough text\033[0m')"
echo ""

# Extended underline styles (DEC private mode)
echo "Underline Styles (if supported):"
echo "  Single:    $(printf '\033[4:1mSingle underline\033[0m')"
echo "  Double:    $(printf '\033[4:2mDouble underline\033[0m')"
echo "  Curly:     $(printf '\033[4:3mCurly underline\033[0m')"
echo "  Dotted:    $(printf '\033[4:4mDotted underline\033[0m')"
echo "  Dashed:    $(printf '\033[4:5mDashed underline\033[0m')"
echo ""

# Overline (less commonly supported)
echo "Overline (if supported):"
echo "  Overline:  $(printf '\033[53mOverlined text\033[0m')"
echo ""

# Foreground colors
echo "16 Foreground Colors:"
echo "  Normal: $(for i in {30..37}; do printf '\033[%dm●\033[0m ' $i; done)"
echo "  Bright: $(for i in {90..97}; do printf '\033[%dm●\033[0m ' $i; done)"
echo ""

# Background colors
echo "16 Background Colors:"
echo "  Normal: $(for i in {40..47}; do printf '\033[%dm  \033[0m' $i; done)"
echo "  Bright: $(for i in {100..107}; do printf '\033[%dm  \033[0m' $i; done)"
echo ""

# 256 colors (sample)
echo "256 Colors (16 samples):"
for row in 0 4 8 12; do
  echo -n "  "
  for col in {0..3}; do
    code=$((row * 4 + col + 16))
    printf '\033[48;5;%dm  \033[0m' $code
  done
  echo ""
done
echo ""

# True color (24-bit RGB)
echo "True Color Gradient (24-bit RGB, if supported):"
echo -n "  "
for i in {0..31}; do
  r=$((i * 8))
  g=$((255 - i * 8))
  b=$((i * 4))
  printf '\033[48;2;%d;%d;%dm  \033[0m' $r $g $b
done
echo ""
echo ""

# OSC 8 Hyperlinks
echo "OSC 8 Hyperlinks (if supported):"
printf "  \033]8;;https://example.com\033\\\\Click here (example.com)\033]8;;\033\\\\"
echo ""
echo ""

# Cursor styles
echo "Cursor Styles (demo, if supported):"
echo "  Block:     $(printf '\033[2 q\033[?25h(block shown briefly)\033[0 q')"
echo "  Underline: $(printf '\033[4 q(underline cursor shown briefly)\033[0 q')"
echo "  Bar:       $(printf '\033[6 q(bar cursor shown briefly)\033[0 q')"
echo ""

# Screen modes (just show, don't actually change)
echo "Screen Modes (informational):"
echo "  Alternate screen: \\033[?1049h (enable) / \\033[?1049l (disable)"
echo "  Cursor visible:   \\033[?25h (show) / \\033[?25l (hide)"
echo ""

# Bracketed paste (informational)
echo "Bracketed Paste Mode (informational):"
echo "  Enable:  \\033[?2004h"
echo "  Disable: \\033[?2004l"
echo ""

# Mouse tracking (informational)
echo "Mouse Tracking (informational):"
echo "  Enable:  \\033[?1000h (basic) / \\033[?1002h (drag) / \\033[?1006h (SGR)"
echo "  Disable: \\033[?1000l"
echo ""

echo "=========================="
echo "Test complete. Not all features work in all terminals."
