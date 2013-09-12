assert (false = (Rep.string_match "ac|bd" "adc" 0));;
assert (Rep.string_match "(abb)|(dxc)" "abdxc" 0);;
assert (Rep.string_match "acb*a" "acbbbbba" 0);;
assert (false = (Rep.string_match "abc*a" "abd" 0));;
assert (Rep.string_match "aws?a" "awsa" 0);;
assert (false = (Rep.string_match "aws?a" "awn" 0));;
assert (Rep.string_match "awwwc+n" "awwwcn" 0);;
assert (false = (Rep.string_match "awx+n" "awn" 0));;
