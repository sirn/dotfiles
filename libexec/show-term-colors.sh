#!/usr/bin/env bash
# Display 256 color palette in a 16x16 grid

echo "256 colors (16x16 grid):"
for row in {0..15}; do
  for col in {0..15}; do
    code=$((row * 16 + col))
    printf '\033[48;5;%dm%4d\033[0m' $code $code
  done
  echo
done

echo ""
echo "Grayscale ramp (232-255):"
for i in {232..255}; do
  printf '\033[48;5;%dm%4d\033[0m' $i $i
done
echo ""
echo ""

echo "Base16 colors:"
echo -n "Normal:  "
for i in {0..7}; do
  printf '\033[48;5;%dm%4d\033[0m' $i $i
done
echo ""
echo -n "Bright:  "
for i in {8..15}; do
  printf '\033[48;5;%dm%4d\033[0m' $i $i
done
echo ""
