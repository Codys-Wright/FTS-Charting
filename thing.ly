\include "english.ly"
\version "2.24.0"

rhythm = {
  \relative {
  e'' f g a b, c d e
} }

\new Staff {
  \numericTimeSignature
  \time 4/4
  \rhythm
}