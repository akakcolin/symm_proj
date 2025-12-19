module hall
  use accuracy
  use constants
  implicit none
  private
  public :: sgsymb, getwyckoff

contains
  
  subroutine sgsymb(hall, num, schn, hm_symbol, hall_num)
    ! !INPUT/OUTPUT PARAMETERS:
    !   hall : Hall symbol (in,character(20))
    !   num  : space group number (out,character(20))
    !   schn : Schoenflies symbol (out,character(20))
    !   hall_num : Hall symbol num (out,character(20))
    ! !DESCRIPTION:
    !   Returns the space group number, Schoenflies and Hall symbols given the
    !   Hall symbol. The routine is case-sensitive. With acknowledgements
    !   to Ralf W. Grosse-Kunstleve and the tables available at
    !   {	t http://cci.lbl.gov/sginfo/}.
    !
    ! !REVISION HISTORY:
    !   Created October 2006 (JKD)
    !EOP
    !BOC
    implicit none
    ! arguments
    character(20), intent(in) :: hall
    integer, intent(out) :: num
    character(20), intent(out) :: schn
    character(20), intent(out) :: hm_symbol
    integer, intent(out) :: hall_num

    select case(trim(adjustl(hall)))

    case('P 1')
        num=1
        schn='C1^1'
        hm_symbol='P 1'
        hall_num=1
    case('-P 1')
        num=2
        schn='Ci^1'
        hm_symbol='P -1'
        hall_num=2
    case('P 2y')
        num=3
        schn='C2^1'
        hm_symbol='P 1 2 1'
        hall_num=3
    case('P 2')
        num=3
        schn='C2^1'
        hm_symbol='P 1 1 2'
        hall_num=4
    case('P 2x')
        num=3
        schn='C2^1'
        hm_symbol='P 2 1 1'
        hall_num=5
    case('P 2yb')
        num=4
        schn='C2^2'
        hm_symbol='P 1 2_1 1'
        hall_num=6
    case('P 2c')
        num=4
        schn='C2^2'
        hm_symbol='P 1 1 2_1'
        hall_num=7
    case('P 2xa')
        num=4
        schn='C2^2'
        hm_symbol='P 2_1 1 1'
        hall_num=8
    case('C 2y')
        num=5
        schn='C2^3'
        hm_symbol='C 1 2 1'
        hall_num=9
    case('A 2y')
        num=5
        schn='C2^3'
        hm_symbol='A 1 2 1'
        hall_num=10
    case('I 2y')
        num=5
        schn='C2^3'
        hm_symbol='I 1 2 1'
        hall_num=11
    case('A 2')
        num=5
        schn='C2^3'
        hm_symbol='A 1 1 2'
        hall_num=12
    case('B 2')
        num=5
        schn='C2^3'
        hm_symbol='B 1 1 2'
        hall_num=13
    case('I 2')
        num=5
        schn='C2^3'
        hm_symbol='I 1 1 2'
        hall_num=14
    case('B 2x')
        num=5
        schn='C2^3'
        hm_symbol='B 2 1 1'
        hall_num=15
    case('C 2x')
        num=5
        schn='C2^3'
        hm_symbol='C 2 1 1'
        hall_num=16
    case('I 2x')
        num=5
        schn='C2^3'
        hm_symbol='I 2 1 1'
        hall_num=17
    case('P -2y')
        num=6
        schn='Cs^1'
        hm_symbol='P 1 m 1'
        hall_num=18
    case('P -2')
        num=6
        schn='Cs^1'
        hm_symbol='P 1 1 m'
        hall_num=19
    case('P -2x')
        num=6
        schn='Cs^1'
        hm_symbol='P m 1 1'
        hall_num=20
    case('P -2yc')
        num=7
        schn='Cs^2'
        hm_symbol='P 1 c 1'
        hall_num=21
    case('P -2yac')
        num=7
        schn='Cs^2'
        hm_symbol='P 1 n 1'
        hall_num=22
    case('P -2ya')
        num=7
        schn='Cs^2'
        hm_symbol='P 1 a 1'
        hall_num=23
    case('P -2a')
        num=7
        schn='Cs^2'
        hm_symbol='P 1 1 a'
        hall_num=24
    case('P -2ab')
        num=7
        schn='Cs^2'
        hm_symbol='P 1 1 n'
        hall_num=25
    case('P -2b')
        num=7
        schn='Cs^2'
        hm_symbol='P 1 1 b'
        hall_num=26
    case('P -2xb')
        num=7
        schn='Cs^2'
        hm_symbol='P b 1 1'
        hall_num=27
    case('P -2xbc')
        num=7
        schn='Cs^2'
        hm_symbol='P n 1 1'
        hall_num=28
    case('P -2xc')
        num=7
        schn='Cs^2'
        hm_symbol='P c 1 1'
        hall_num=29
    case('C -2y')
        num=8
        schn='Cs^3'
        hm_symbol='C 1 m 1'
        hall_num=30
    case('A -2y')
        num=8
        schn='Cs^3'
        hm_symbol='A 1 m 1'
        hall_num=31
    case('I -2y')
        num=8
        schn='Cs^3'
        hm_symbol='I 1 m 1'
        hall_num=32
    case('A -2')
        num=8
        schn='Cs^3'
        hm_symbol='A 1 1 m'
        hall_num=33
    case('B -2')
        num=8
        schn='Cs^3'
        hm_symbol='B 1 1 m'
        hall_num=34
    case('I -2')
        num=8
        schn='Cs^3'
        hm_symbol='I 1 1 m'
        hall_num=35
    case('B -2x')
        num=8
        schn='Cs^3'
        hm_symbol='B m 1 1'
        hall_num=36
    case('C -2x')
        num=8
        schn='Cs^3'
        hm_symbol='C m 1 1'
        hall_num=37
    case('I -2x')
        num=8
        schn='Cs^3'
        hm_symbol='I m 1 1'
        hall_num=38
    case('C -2yc')
        num=9
        schn='Cs^4'
        hm_symbol='C 1 c 1'
        hall_num=39
    case('A -2yac')
        num=9
        schn='Cs^4'
        hm_symbol='A 1 n 1'
        hall_num=40
    case('I -2ya')
        num=9
        schn='Cs^4'
        hm_symbol='I 1 a 1'
        hall_num=41
    case('A -2ya')
        num=9
        schn='Cs^4'
        hm_symbol='A 1 a 1'
        hall_num=42
    case('C -2ybc')
        num=9
        schn='Cs^4'
        hm_symbol='C 1 n 1'
        hall_num=43
    case('I -2yc')
        num=9
        schn='Cs^4'
        hm_symbol='I 1 c 1'
        hall_num=44
    case('A -2a')
        num=9
        schn='Cs^4'
        hm_symbol='A 1 1 a'
        hall_num=45
    case('B -2bc')
        num=9
        schn='Cs^4'
        hm_symbol='B 1 1 n'
        hall_num=46
    case('I -2b')
        num=9
        schn='Cs^4'
        hm_symbol='I 1 1 b'
        hall_num=47
    case('B -2b')
        num=9
        schn='Cs^4'
        hm_symbol='B 1 1 b'
        hall_num=48
    case('A -2ac')
        num=9
        schn='Cs^4'
        hm_symbol='A 1 1 n'
        hall_num=49
    case('I -2a')
        num=9
        schn='Cs^4'
        hm_symbol='I 1 1 a'
        hall_num=50
    case('B -2xb')
        num=9
        schn='Cs^4'
        hm_symbol='B b 1 1'
        hall_num=51
    case('C -2xbc')
        num=9
        schn='Cs^4'
        hm_symbol='C n 1 1'
        hall_num=52
    case('I -2xc')
        num=9
        schn='Cs^4'
        hm_symbol='I c 1 1'
        hall_num=53
    case('C -2xc')
        num=9
        schn='Cs^4'
        hm_symbol='C c 1 1'
        hall_num=54
    case('B -2xbc')
        num=9
        schn='Cs^4'
        hm_symbol='B n 1 1'
        hall_num=55
    case('I -2xb')
        num=9
        schn='Cs^4'
        hm_symbol='I b 1 1'
        hall_num=56
    case('-P 2y')
        num=10
        schn='C2h^1'
        hm_symbol='P 1 2/m 1'
        hall_num=57
    case('-P 2')
        num=10
        schn='C2h^1'
        hm_symbol='P 1 1 2/m'
        hall_num=58
    case('-P 2x')
        num=10
        schn='C2h^1'
        hm_symbol='P 2/m 1 1'
        hall_num=59
    case('-P 2yb')
        num=11
        schn='C2h^2'
        hm_symbol='P 1 2_1/m 1'
        hall_num=60
    case('-P 2c')
        num=11
        schn='C2h^2'
        hm_symbol='P 1 1 2_1/m'
        hall_num=61
    case('-P 2xa')
        num=11
        schn='C2h^2'
        hm_symbol='P 2_1/m 1 1'
        hall_num=62
    case('-C 2y')
        num=12
        schn='C2h^3'
        hm_symbol='C 1 2/m 1'
        hall_num=63
    case('-A 2y')
        num=12
        schn='C2h^3'
        hm_symbol='A 1 2/m 1'
        hall_num=64
    case('-I 2y')
        num=12
        schn='C2h^3'
        hm_symbol='I 1 2/m 1'
        hall_num=65
    case('-A 2')
        num=12
        schn='C2h^3'
        hm_symbol='A 1 1 2/m'
        hall_num=66
    case('-B 2')
        num=12
        schn='C2h^3'
        hm_symbol='B 1 1 2/m'
        hall_num=67
    case('-I 2')
        num=12
        schn='C2h^3'
        hm_symbol='I 1 1 2/m'
        hall_num=68
    case('-B 2x')
        num=12
        schn='C2h^3'
        hm_symbol='B 2/m 1 1'
        hall_num=69
    case('-C 2x')
        num=12
        schn='C2h^3'
        hm_symbol='C 2/m 1 1'
        hall_num=70
    case('-I 2x')
        num=12
        schn='C2h^3'
        hm_symbol='I 2/m 1 1'
        hall_num=71
    case('-P 2yc')
        num=13
        schn='C2h^4'
        hm_symbol='P 1 2/c 1'
        hall_num=72
    case('-P 2yac')
        num=13
        schn='C2h^4'
        hm_symbol='P 1 2/n 1'
        hall_num=73
    case('-P 2ya')
        num=13
        schn='C2h^4'
        hm_symbol='P 1 2/a 1'
        hall_num=74
    case('-P 2a')
        num=13
        schn='C2h^4'
        hm_symbol='P 1 1 2/a'
        hall_num=75
    case('-P 2ab')
        num=13
        schn='C2h^4'
        hm_symbol='P 1 1 2/n'
        hall_num=76
    case('-P 2b')
        num=13
        schn='C2h^4'
        hm_symbol='P 1 1 2/b'
        hall_num=77
    case('-P 2xb')
        num=13
        schn='C2h^4'
        hm_symbol='P 2/b 1 1'
        hall_num=78
    case('-P 2xbc')
        num=13
        schn='C2h^4'
        hm_symbol='P 2/n 1 1'
        hall_num=79
    case('-P 2xc')
        num=13
        schn='C2h^4'
        hm_symbol='P 2/c 1 1'
        hall_num=80
    case('-P 2ybc')
        num=14
        schn='C2h^5'
        hm_symbol='P 1 2_1/c 1'
        hall_num=81
    case('-P 2yn')
        num=14
        schn='C2h^5'
        hm_symbol='P 1 2_1/n 1'
        hall_num=82
    case('-P 2yab')
        num=14
        schn='C2h^5'
        hm_symbol='P 1 2_1/a 1'
        hall_num=83
    case('-P 2ac')
        num=14
        schn='C2h^5'
        hm_symbol='P 1 1 2_1/a'
        hall_num=84
    case('-P 2n')
        num=14
        schn='C2h^5'
        hm_symbol='P 1 1 2_1/n'
        hall_num=85
    case('-P 2bc')
        num=14
        schn='C2h^5'
        hm_symbol='P 1 1 2_1/b'
        hall_num=86
    case('-P 2xab')
        num=14
        schn='C2h^5'
        hm_symbol='P 2_1/b 1 1'
        hall_num=87
    case('-P 2xn')
        num=14
        schn='C2h^5'
        hm_symbol='P 2_1/n 1 1'
        hall_num=88
    case('-P 2xac')
        num=14
        schn='C2h^5'
        hm_symbol='P 2_1/c 1 1'
        hall_num=89
    case('-C 2yc')
        num=15
        schn='C2h^6'
        hm_symbol='C 1 2/c 1'
        hall_num=90
    case('-A 2yac')
        num=15
        schn='C2h^6'
        hm_symbol='A 1 2/n 1'
        hall_num=91
    case('-I 2ya')
        num=15
        schn='C2h^6'
        hm_symbol='I 1 2/a 1'
        hall_num=92
    case('-A 2ya')
        num=15
        schn='C2h^6'
        hm_symbol='A 1 2/a 1'
        hall_num=93
    case('-C 2ybc')
        num=15
        schn='C2h^6'
        hm_symbol='C 1 2/n 1'
        hall_num=94
    case('-I 2yc')
        num=15
        schn='C2h^6'
        hm_symbol='I 1 2/c 1'
        hall_num=95
    case('-A 2a')
        num=15
        schn='C2h^6'
        hm_symbol='A 1 1 2/a'
        hall_num=96
    case('-B 2bc')
        num=15
        schn='C2h^6'
        hm_symbol='B 1 1 2/n'
        hall_num=97
    case('-I 2b')
        num=15
        schn='C2h^6'
        hm_symbol='I 1 1 2/b'
        hall_num=98
    case('-B 2b')
        num=15
        schn='C2h^6'
        hm_symbol='B 1 1 2/b'
        hall_num=99
    case('-A 2ac')
        num=15
        schn='C2h^6'
        hm_symbol='A 1 1 2/n'
        hall_num=100
    case('-I 2a')
        num=15
        schn='C2h^6'
        hm_symbol='I 1 1 2/a'
        hall_num=101
    case('-B 2xb')
        num=15
        schn='C2h^6'
        hm_symbol='B 2/b 1 1'
        hall_num=102
    case('-C 2xbc')
        num=15
        schn='C2h^6'
        hm_symbol='C 2/n 1 1'
        hall_num=103
    case('-I 2xc')
        num=15
        schn='C2h^6'
        hm_symbol='I 2/c 1 1'
        hall_num=104
    case('-C 2xc')
        num=15
        schn='C2h^6'
        hm_symbol='C 2/c 1 1'
        hall_num=105
    case('-B 2xbc')
        num=15
        schn='C2h^6'
        hm_symbol='B 2/n 1 1'
        hall_num=106
    case('-I 2xb')
        num=15
        schn='C2h^6'
        hm_symbol='I 2/b 1 1'
        hall_num=107
    case('P 2 2')
        num=16
        schn='D2^1'
        hm_symbol='P 2 2 2'
        hall_num=108
    case('P 2c 2')
        num=17
        schn='D2^2'
        hm_symbol='P 2 2 2_1'
        hall_num=109
    case('P 2a 2a')
        num=17
        schn='D2^2'
        hm_symbol='P 2_1 2 2'
        hall_num=110
    case('P 2 2b')
        num=17
        schn='D2^2'
        hm_symbol='P 2 2_1 2'
        hall_num=111
    case('P 2 2ab')
        num=18
        schn='D2^3'
        hm_symbol='P 2_1 2_1 2'
        hall_num=112
    case('P 2bc 2')
        num=18
        schn='D2^3'
        hm_symbol='P 2 2_1 2_1'
        hall_num=113
    case('P 2ac 2ac')
        num=18
        schn='D2^3'
        hm_symbol='P 2_1 2 2_1'
        hall_num=114
    case('P 2ac 2ab')
        num=19
        schn='D2^4'
        hm_symbol='P 2_1 2_1 2_1'
        hall_num=115
    case('C 2c 2')
        num=20
        schn='D2^5'
        hm_symbol='C 2 2 2_1'
        hall_num=116
    case('A 2a 2a')
        num=20
        schn='D2^5'
        hm_symbol='A 2_1 2 2'
        hall_num=117
    case('B 2 2b')
        num=20
        schn='D2^5'
        hm_symbol='B 2 2_1 2'
        hall_num=118
    case('C 2 2')
        num=21
        schn='D2^6'
        hm_symbol='C 2 2 2'
        hall_num=119
    case('A 2 2')
        num=21
        schn='D2^6'
        hm_symbol='A 2 2 2'
        hall_num=120
    case('B 2 2')
        num=21
        schn='D2^6'
        hm_symbol='B 2 2 2'
        hall_num=121
    case('F 2 2')
        num=22
        schn='D2^7'
        hm_symbol='F 2 2 2'
        hall_num=122
    case('I 2 2')
        num=23
        schn='D2^8'
        hm_symbol='I 2 2 2'
        hall_num=123
    case('I 2b 2c')
        num=24
        schn='D2^9'
        hm_symbol='I 2_1 2_1 2_1'
        hall_num=124
    case('P 2 -2')
        num=25
        schn='C2v^1'
        hm_symbol='P m m 2'
        hall_num=125
    case('P -2 2')
        num=25
        schn='C2v^1'
        hm_symbol='P 2 m m'
        hall_num=126
    case('P -2 -2')
        num=25
        schn='C2v^1'
        hm_symbol='P m 2 m'
        hall_num=127
    case('P 2c -2')
        num=26
        schn='C2v^2'
        hm_symbol='P m c 2_1'
        hall_num=128
    case('P 2c -2c')
        num=26
        schn='C2v^2'
        hm_symbol='P c m 2_1'
        hall_num=129
    case('P -2a 2a')
        num=26
        schn='C2v^2'
        hm_symbol='P 2_1 m a'
        hall_num=130
    case('P -2 2a')
        num=26
        schn='C2v^2'
        hm_symbol='P 2_1 a m'
        hall_num=131
    case('P -2 -2b')
        num=26
        schn='C2v^2'
        hm_symbol='P b 2_1 m'
        hall_num=132
    case('P -2b -2')
        num=26
        schn='C2v^2'
        hm_symbol='P m 2_1 b'
        hall_num=133
    case('P 2 -2c')
        num=27
        schn='C2v^3'
        hm_symbol='P c c 2'
        hall_num=134
    case('P -2a 2')
        num=27
        schn='C2v^3'
        hm_symbol='P 2 a a'
        hall_num=135
    case('P -2b -2b')
        num=27
        schn='C2v^3'
        hm_symbol='P b 2 b'
        hall_num=136
    case('P 2 -2a')
        num=28
        schn='C2v^4'
        hm_symbol='P m a 2'
        hall_num=137
    case('P 2 -2b')
        num=28
        schn='C2v^4'
        hm_symbol='P b m 2'
        hall_num=138
    case('P -2b 2')
        num=28
        schn='C2v^4'
        hm_symbol='P 2 m b'
        hall_num=139
    case('P -2c 2')
        num=28
        schn='C2v^4'
        hm_symbol='P 2 c m'
        hall_num=140
    case('P -2c -2c')
        num=28
        schn='C2v^4'
        hm_symbol='P c 2 m'
        hall_num=141
    case('P -2a -2a')
        num=28
        schn='C2v^4'
        hm_symbol='P m 2 a'
        hall_num=142
    case('P 2c -2ac')
        num=29
        schn='C2v^5'
        hm_symbol='P c a 2_1'
        hall_num=143
    case('P 2c -2b')
        num=29
        schn='C2v^5'
        hm_symbol='P b c 2_1'
        hall_num=144
    case('P -2b 2a')
        num=29
        schn='C2v^5'
        hm_symbol='P 2_1 a b'
        hall_num=145
    case('P -2ac 2a')
        num=29
        schn='C2v^5'
        hm_symbol='P 2_1 c a'
        hall_num=146
    case('P -2bc -2c')
        num=29
        schn='C2v^5'
        hm_symbol='P c 2_1 b'
        hall_num=147
    case('P -2a -2ab')
        num=29
        schn='C2v^5'
        hm_symbol='P b 2_1 a'
        hall_num=148
    case('P 2 -2bc')
        num=30
        schn='C2v^6'
        hm_symbol='P n c 2'
        hall_num=149
    case('P 2 -2ac')
        num=30
        schn='C2v^6'
        hm_symbol='P c n 2'
        hall_num=150
    case('P -2ac 2')
        num=30
        schn='C2v^6'
        hm_symbol='P 2 n a'
        hall_num=151
    case('P -2ab 2')
        num=30
        schn='C2v^6'
        hm_symbol='P 2 a n'
        hall_num=152
    case('P -2ab -2ab')
        num=30
        schn='C2v^6'
        hm_symbol='P b 2 n'
        hall_num=153
    case('P -2bc -2bc')
        num=30
        schn='C2v^6'
        hm_symbol='P n 2 b'
        hall_num=154
    case('P 2ac -2')
        num=31
        schn='C2v^7'
        hm_symbol='P m n 2_1'
        hall_num=155
    case('P 2bc -2bc')
        num=31
        schn='C2v^7'
        hm_symbol='P n m 2_1'
        hall_num=156
    case('P -2ab 2ab')
        num=31
        schn='C2v^7'
        hm_symbol='P 2_1 m n'
        hall_num=157
    case('P -2 2ac')
        num=31
        schn='C2v^7'
        hm_symbol='P 2_1 n m'
        hall_num=158
    case('P -2 -2bc')
        num=31
        schn='C2v^7'
        hm_symbol='P n 2_1 m'
        hall_num=159
    case('P -2ab -2')
        num=31
        schn='C2v^7'
        hm_symbol='P m 2_1 n'
        hall_num=160
    case('P 2 -2ab')
        num=32
        schn='C2v^8'
        hm_symbol='P b a 2'
        hall_num=161
    case('P -2bc 2')
        num=32
        schn='C2v^8'
        hm_symbol='P 2 c b'
        hall_num=162
    case('P -2ac -2ac')
        num=32
        schn='C2v^8'
        hm_symbol='P c 2 a'
        hall_num=163
    case('P 2c -2n')
        num=33
        schn='C2v^9'
        hm_symbol='P n a 2_1'
        hall_num=164
    case('P 2c -2ab')
        num=33
        schn='C2v^9'
        hm_symbol='P b n 2_1'
        hall_num=165
    case('P -2bc 2a')
        num=33
        schn='C2v^9'
        hm_symbol='P 2_1 n b'
        hall_num=166
    case('P -2n 2a')
        num=33
        schn='C2v^9'
        hm_symbol='P 2_1 c n'
        hall_num=167
    case('P -2n -2ac')
        num=33
        schn='C2v^9'
        hm_symbol='P c 2_1 n'
        hall_num=168
    case('P -2ac -2n')
        num=33
        schn='C2v^9'
        hm_symbol='P n 2_1 a'
        hall_num=169
    case('P 2 -2n')
        num=34
        schn='C2v^10'
        hm_symbol='P n n 2'
        hall_num=170
    case('P -2n 2')
        num=34
        schn='C2v^10'
        hm_symbol='P 2 n n'
        hall_num=171
    case('P -2n -2n')
        num=34
        schn='C2v^10'
        hm_symbol='P n 2 n'
        hall_num=172
    case('C 2 -2')
        num=35
        schn='C2v^11'
        hm_symbol='C m m 2'
        hall_num=173
    case('A -2 2')
        num=35
        schn='C2v^11'
        hm_symbol='A 2 m m'
        hall_num=174
    case('B -2 -2')
        num=35
        schn='C2v^11'
        hm_symbol='B m 2 m'
        hall_num=175
    case('C 2c -2')
        num=36
        schn='C2v^12'
        hm_symbol='C m c 2_1'
        hall_num=176
    case('C 2c -2c')
        num=36
        schn='C2v^12'
        hm_symbol='C c m 2_1'
        hall_num=177
    case('A -2a 2a')
        num=36
        schn='C2v^12'
        hm_symbol='A 2_1 m a'
        hall_num=178
    case('A -2 2a')
        num=36
        schn='C2v^12'
        hm_symbol='A 2_1 a m'
        hall_num=179
    case('B -2 -2b')
        num=36
        schn='C2v^12'
        hm_symbol='B b 2_1 m'
        hall_num=180
    case('B -2b -2')
        num=36
        schn='C2v^12'
        hm_symbol='B m 2_1 b'
        hall_num=181
    case('C 2 -2c')
        num=37
        schn='C2v^13'
        hm_symbol='C c c 2'
        hall_num=182
    case('A -2a 2')
        num=37
        schn='C2v^13'
        hm_symbol='A 2 a a'
        hall_num=183
    case('B -2b -2b')
        num=37
        schn='C2v^13'
        hm_symbol='B b 2 b'
        hall_num=184
    case('A 2 -2')
        num=38
        schn='C2v^14'
        hm_symbol='A m m 2'
        hall_num=185
    case('B 2 -2')
        num=38
        schn='C2v^14'
        hm_symbol='B m m 2'
        hall_num=186
    case('B -2 2')
        num=38
        schn='C2v^14'
        hm_symbol='B 2 m m'
        hall_num=187
    case('C -2 2')
        num=38
        schn='C2v^14'
        hm_symbol='C 2 m m'
        hall_num=188
    case('C -2 -2')
        num=38
        schn='C2v^14'
        hm_symbol='C m 2 m'
        hall_num=189
    case('A -2 -2')
        num=38
        schn='C2v^14'
        hm_symbol='A m 2 m'
        hall_num=190
    case('A 2 -2c')
        num=39
        schn='C2v^15'
        hm_symbol='A e m 2'
        hall_num=191
    case('B 2 -2c')
        num=39
        schn='C2v^15'
        hm_symbol='B m e 2'
        hall_num=192
    case('B -2c 2')
        num=39
        schn='C2v^15'
        hm_symbol='B 2 e m'
        hall_num=193
    case('C -2b 2')
        num=39
        schn='C2v^15'
        hm_symbol='C 2 m e'
        hall_num=194
    case('C -2b -2b')
        num=39
        schn='C2v^15'
        hm_symbol='C m 2 e'
        hall_num=195
    case('A -2c -2c')
        num=39
        schn='C2v^15'
        hm_symbol='A e 2 m'
        hall_num=196
    case('A 2 -2a')
        num=40
        schn='C2v^16'
        hm_symbol='A m a 2'
        hall_num=197
    case('B 2 -2b')
        num=40
        schn='C2v^16'
        hm_symbol='B b m 2'
        hall_num=198
    case('B -2b 2')
        num=40
        schn='C2v^16'
        hm_symbol='B 2 m b'
        hall_num=199
    case('C -2c 2')
        num=40
        schn='C2v^16'
        hm_symbol='C 2 c m'
        hall_num=200
    case('C -2c -2c')
        num=40
        schn='C2v^16'
        hm_symbol='C c 2 m'
        hall_num=201
    case('A -2a -2a')
        num=40
        schn='C2v^16'
        hm_symbol='A m 2 a'
        hall_num=202
    case('A 2 -2ac')
        num=41
        schn='C2v^17'
        hm_symbol='A e a 2'
        hall_num=203
    case('B 2 -2bc')
        num=41
        schn='C2v^17'
        hm_symbol='B b e 2'
        hall_num=204
    case('B -2bc 2')
        num=41
        schn='C2v^17'
        hm_symbol='B 2 e b'
        hall_num=205
    case('C -2bc 2')
        num=41
        schn='C2v^17'
        hm_symbol='C 2 c e'
        hall_num=206
    case('C -2bc -2bc')
        num=41
        schn='C2v^17'
        hm_symbol='C c 2 e'
        hall_num=207
    case('A -2ac -2ac')
        num=41
        schn='C2v^17'
        hm_symbol='A e 2 a'
        hall_num=208
    case('F 2 -2')
        num=42
        schn='C2v^18'
        hm_symbol='F m m 2'
        hall_num=209
    case('F -2 2')
        num=42
        schn='C2v^18'
        hm_symbol='F 2 m m'
        hall_num=210
    case('F -2 -2')
        num=42
        schn='C2v^18'
        hm_symbol='F m 2 m'
        hall_num=211
    case('F 2 -2d')
        num=43
        schn='C2v^19'
        hm_symbol='F d d 2'
        hall_num=212
    case('F -2d 2')
        num=43
        schn='C2v^19'
        hm_symbol='F 2 d d'
        hall_num=213
    case('F -2d -2d')
        num=43
        schn='C2v^19'
        hm_symbol='F d 2 d'
        hall_num=214
    case('I 2 -2')
        num=44
        schn='C2v^20'
        hm_symbol='I m m 2'
        hall_num=215
    case('I -2 2')
        num=44
        schn='C2v^20'
        hm_symbol='I 2 m m'
        hall_num=216
    case('I -2 -2')
        num=44
        schn='C2v^20'
        hm_symbol='I m 2 m'
        hall_num=217
    case('I 2 -2c')
        num=45
        schn='C2v^21'
        hm_symbol='I b a 2'
        hall_num=218
    case('I -2a 2')
        num=45
        schn='C2v^21'
        hm_symbol='I 2 c b'
        hall_num=219
    case('I -2b -2b')
        num=45
        schn='C2v^21'
        hm_symbol='I c 2 a'
        hall_num=220
    case('I 2 -2a')
        num=46
        schn='C2v^22'
        hm_symbol='I m a 2'
        hall_num=221
    case('I 2 -2b')
        num=46
        schn='C2v^22'
        hm_symbol='I b m 2'
        hall_num=222
    case('I -2b 2')
        num=46
        schn='C2v^22'
        hm_symbol='I 2 m b'
        hall_num=223
    case('I -2c 2')
        num=46
        schn='C2v^22'
        hm_symbol='I 2 c m'
        hall_num=224
    case('I -2c -2c')
        num=46
        schn='C2v^22'
        hm_symbol='I c 2 m'
        hall_num=225
    case('I -2a -2a')
        num=46
        schn='C2v^22'
        hm_symbol='I m 2 a'
        hall_num=226
    case('-P 2 2')
        num=47
        schn='D2h^1'
        hm_symbol='P 2/m 2/m 2/m'
        hall_num=227
    case('P 2 2 -1n')
        num=48
        schn='D2h^2'
        hm_symbol='P 2/n 2/n 2/n'
        hall_num=228
    case('-P 2ab 2bc')
        num=48
        schn='D2h^2'
        hm_symbol='P 2/n 2/n 2/n'
        hall_num=229
    case('-P 2 2c')
        num=49
        schn='D2h^3'
        hm_symbol='P 2/c 2/c 2/m'
        hall_num=230
    case('-P 2a 2')
        num=49
        schn='D2h^3'
        hm_symbol='P 2/m 2/a 2/a'
        hall_num=231
    case('-P 2b 2b')
        num=49
        schn='D2h^3'
        hm_symbol='P 2/b 2/m 2/b'
        hall_num=232
    case('P 2 2 -1ab')
        num=50
        schn='D2h^4'
        hm_symbol='P 2/b 2/a 2/n'
        hall_num=233
    case('-P 2ab 2b')
        num=50
        schn='D2h^4'
        hm_symbol='P 2/b 2/a 2/n'
        hall_num=234
    case('P 2 2 -1bc')
        num=50
        schn='D2h^4'
        hm_symbol='P 2/n 2/c 2/b'
        hall_num=235
    case('-P 2b 2bc')
        num=50
        schn='D2h^4'
        hm_symbol='P 2/n 2/c 2/b'
        hall_num=236
    case('P 2 2 -1ac')
        num=50
        schn='D2h^4'
        hm_symbol='P 2/c 2/n 2/a'
        hall_num=237
    case('-P 2a 2c')
        num=50
        schn='D2h^4'
        hm_symbol='P 2/c 2/n 2/a'
        hall_num=238
    case('-P 2a 2a')
        num=51
        schn='D2h^5'
        hm_symbol='P 2_1/m 2/m 2/a'
        hall_num=239
    case('-P 2b 2')
        num=51
        schn='D2h^5'
        hm_symbol='P 2/m 2_1/m 2/b'
        hall_num=240
    case('-P 2 2b')
        num=51
        schn='D2h^5'
        hm_symbol='P 2/b 2_1/m 2/m'
        hall_num=241
    case('-P 2c 2c')
        num=51
        schn='D2h^5'
        hm_symbol='P 2/c 2/m 2_1/m'
        hall_num=242
    case('-P 2c 2')
        num=51
        schn='D2h^5'
        hm_symbol='P 2/m 2/c 2_1/m'
        hall_num=243
    case('-P 2 2a')
        num=51
        schn='D2h^5'
        hm_symbol='P 2_1/m 2/a 2/m'
        hall_num=244
    case('-P 2a 2bc')
        num=52
        schn='D2h^6'
        hm_symbol='P 2/n 2_1/n 2/a'
        hall_num=245
    case('-P 2b 2n')
        num=52
        schn='D2h^6'
        hm_symbol='P 2_1/n 2/n 2/b'
        hall_num=246
    case('-P 2n 2b')
        num=52
        schn='D2h^6'
        hm_symbol='P 2/b 2/n 2_1/n'
        hall_num=247
    case('-P 2ab 2c')
        num=52
        schn='D2h^6'
        hm_symbol='P 2/c 2_1/n 2/n'
        hall_num=248
    case('-P 2ab 2n')
        num=52
        schn='D2h^6'
        hm_symbol='P 2_1/n 2/c 2/n'
        hall_num=249
    case('-P 2n 2bc')
        num=52
        schn='D2h^6'
        hm_symbol='P 2/n 2/a 2_1/n'
        hall_num=250
    case('-P 2ac 2')
        num=53
        schn='D2h^7'
        hm_symbol='P 2/m 2/n 2_1/a'
        hall_num=251
    case('-P 2bc 2bc')
        num=53
        schn='D2h^7'
        hm_symbol='P 2/n 2/m 2_1/b'
        hall_num=252
    case('-P 2ab 2ab')
        num=53
        schn='D2h^7'
        hm_symbol='P 2_1/b 2/m 2/n'
        hall_num=253
    case('-P 2 2ac')
        num=53
        schn='D2h^7'
        hm_symbol='P 2_1/c 2/n 2/m'
        hall_num=254
    case('-P 2 2bc')
        num=53
        schn='D2h^7'
        hm_symbol='P 2/n 2_1/c 2/m'
        hall_num=255
    case('-P 2ab 2')
        num=53
        schn='D2h^7'
        hm_symbol='P 2/m 2_1/a 2/n'
        hall_num=256
    case('-P 2a 2ac')
        num=54
        schn='D2h^8'
        hm_symbol='P 2_1/c 2/c 2/a'
        hall_num=257
    case('-P 2b 2c')
        num=54
        schn='D2h^8'
        hm_symbol='P 2/c 2_1/c 2/b'
        hall_num=258
    case('-P 2a 2b')
        num=54
        schn='D2h^8'
        hm_symbol='P 2/b 2_1/a 2/a'
        hall_num=259
    case('-P 2ac 2c')
        num=54
        schn='D2h^8'
        hm_symbol='P 2/c 2/a 2_1/a'
        hall_num=260
    case('-P 2bc 2b')
        num=54
        schn='D2h^8'
        hm_symbol='P 2/b 2/c 2_1/b'
        hall_num=261
    case('-P 2b 2ab')
        num=54
        schn='D2h^8'
        hm_symbol='P 2_1/b 2/a 2/b'
        hall_num=262
    case('-P 2 2ab')
        num=55
        schn='D2h^9'
        hm_symbol='P 2_1/b 2_1/a 2/m'
        hall_num=263
    case('-P 2bc 2')
        num=55
        schn='D2h^9'
        hm_symbol='P 2/m 2_1/c 2_1/b'
        hall_num=264
    case('-P 2ac 2ac')
        num=55
        schn='D2h^9'
        hm_symbol='P 2_1/c 2/m 2_1/a'
        hall_num=265
    case('-P 2ab 2ac')
        num=56
        schn='D2h^10'
        hm_symbol='P 2_1/c 2_1/c 2/n'
        hall_num=266
    case('-P 2ac 2bc')
        num=56
        schn='D2h^10'
        hm_symbol='P 2/n 2_1/a 2_1/a'
        hall_num=267
    case('-P 2bc 2ab')
        num=56
        schn='D2h^10'
        hm_symbol='P 2_1/b 2/n 2_1/b'
        hall_num=268
    case('-P 2c 2b')
        num=57
        schn='D2h^11'
        hm_symbol='P 2/b 2_1/c 2_1/m'
        hall_num=269
    case('-P 2c 2ac')
        num=57
        schn='D2h^11'
        hm_symbol='P 2_1/c 2/a 2_1/m'
        hall_num=270
    case('-P 2ac 2a')
        num=57
        schn='D2h^11'
        hm_symbol='P 2_1/m 2/c 2_1/a'
        hall_num=271
    case('-P 2b 2a')
        num=57
        schn='D2h^11'
        hm_symbol='P 2_1/m 2_1/a 2/b'
        hall_num=272
    case('-P 2a 2ab')
        num=57
        schn='D2h^11'
        hm_symbol='P 2_1/b 2_1/m 2/a'
        hall_num=273
    case('-P 2bc 2c')
        num=57
        schn='D2h^11'
        hm_symbol='P 2/c 2_1/m 2_1/b'
        hall_num=274
    case('-P 2 2n')
        num=58
        schn='D2h^12'
        hm_symbol='P 2_1/n 2_1/n 2/m'
        hall_num=275
    case('-P 2n 2')
        num=58
        schn='D2h^12'
        hm_symbol='P 2/m 2_1/n 2_1/n'
        hall_num=276
    case('-P 2n 2n')
        num=58
        schn='D2h^12'
        hm_symbol='P 2_1/n 2/m 2_1/n'
        hall_num=277
    case('P 2 2ab -1ab')
        num=59
        schn='D2h^13'
        hm_symbol='P 2_1/m 2_1/m 2/n'
        hall_num=278
    case('-P 2ab 2a')
        num=59
        schn='D2h^13'
        hm_symbol='P 2_1/m 2_1/m 2/n'
        hall_num=279
    case('P 2bc 2 -1bc')
        num=59
        schn='D2h^13'
        hm_symbol='P 2/n 2_1/m 2_1/m'
        hall_num=280
    case('-P 2c 2bc')
        num=59
        schn='D2h^13'
        hm_symbol='P 2/n 2_1/m 2_1/m'
        hall_num=281
    case('P 2ac 2ac -1ac')
        num=59
        schn='D2h^13'
        hm_symbol='P 2_1/m 2/n 2_1/m'
        hall_num=282
    case('-P 2c 2a')
        num=59
        schn='D2h^13'
        hm_symbol='P 2_1/m 2/n 2_1/m'
        hall_num=283
    case('-P 2n 2ab')
        num=60
        schn='D2h^14'
        hm_symbol='P 2_1/b 2/c 2_1/n'
        hall_num=284
    case('-P 2n 2c')
        num=60
        schn='D2h^14'
        hm_symbol='P 2/c 2_1/a 2_1/n'
        hall_num=285
    case('-P 2a 2n')
        num=60
        schn='D2h^14'
        hm_symbol='P 2_1/n 2_1/c 2/a'
        hall_num=286
    case('-P 2bc 2n')
        num=60
        schn='D2h^14'
        hm_symbol='P 2_1/n 2/a 2_1/b'
        hall_num=287
    case('-P 2ac 2b')
        num=60
        schn='D2h^14'
        hm_symbol='P 2/b 2_1/n 2_1/a'
        hall_num=288
    case('-P 2b 2ac')
        num=60
        schn='D2h^14'
        hm_symbol='P 2_1/c 2_1/n 2/b'
        hall_num=289
    case('-P 2ac 2ab')
        num=61
        schn='D2h^15'
        hm_symbol='P 2_1/b 2_1/c 2_1/a'
        hall_num=290
    case('-P 2bc 2ac')
        num=61
        schn='D2h^15'
        hm_symbol='P 2_1/c 2_1/a 2_1/b'
        hall_num=291
    case('-P 2ac 2n')
        num=62
        schn='D2h^16'
        hm_symbol='P 2_1/n 2_1/m 2_1/a'
        hall_num=292
    case('-P 2bc 2a')
        num=62
        schn='D2h^16'
        hm_symbol='P 2_1/m 2_1/n 2_1/b'
        hall_num=293
    case('-P 2c 2ab')
        num=62
        schn='D2h^16'
        hm_symbol='P 2_1/b 2_1/n 2_1/m'
        hall_num=294
    case('-P 2n 2ac')
        num=62
        schn='D2h^16'
        hm_symbol='P 2_1/c 2_1/m 2_1/n'
        hall_num=295
    case('-P 2n 2a')
        num=62
        schn='D2h^16'
        hm_symbol='P 2_1/m 2_1/c 2_1/n'
        hall_num=296
    case('-P 2c 2n')
        num=62
        schn='D2h^16'
        hm_symbol='P 2_1/n 2_1/a 2_1/m'
        hall_num=297
    case('-C 2c 2')
        num=63
        schn='D2h^17'
        hm_symbol='C 2/m 2/c 2_1/m'
        hall_num=298
    case('-C 2c 2c')
        num=63
        schn='D2h^17'
        hm_symbol='C 2/c 2/m 2_1/m'
        hall_num=299
    case('-A 2a 2a')
        num=63
        schn='D2h^17'
        hm_symbol='A 2_1/m 2/m 2/a'
        hall_num=300
    case('-A 2 2a')
        num=63
        schn='D2h^17'
        hm_symbol='A 2_1/m 2/a 2/m'
        hall_num=301
    case('-B 2 2b')
        num=63
        schn='D2h^17'
        hm_symbol='B 2/b 2_1/m 2/m'
        hall_num=302
    case('-B 2b 2')
        num=63
        schn='D2h^17'
        hm_symbol='B 2/m 2_1/m 2/b'
        hall_num=303
    case('-C 2bc 2')
        num=64
        schn='D2h^18'
        hm_symbol='C 2/m 2/c 2_1/e'
        hall_num=304
    case('-C 2bc 2bc')
        num=64
        schn='D2h^18'
        hm_symbol='C 2/c 2/m 2_1/e'
        hall_num=305
    case('-A 2ac 2ac')
        num=64
        schn='D2h^18'
        hm_symbol='A 2_1/e 2/m 2/a'
        hall_num=306
    case('-A 2 2ac')
        num=64
        schn='D2h^18'
        hm_symbol='A 2_1/e 2/a 2/m'
        hall_num=307
    case('-B 2 2bc')
        num=64
        schn='D2h^18'
        hm_symbol='B 2/b 2_1/e 2/m'
        hall_num=308
    case('-B 2bc 2')
        num=64
        schn='D2h^18'
        hm_symbol='B 2/m 2_1/e 2/b'
        hall_num=309
    case('-C 2 2')
        num=65
        schn='D2h^19'
        hm_symbol='C 2/m 2/m 2/m'
        hall_num=310
    case('-A 2 2')
        num=65
        schn='D2h^19'
        hm_symbol='A 2/m 2/m 2/m'
        hall_num=311
    case('-B 2 2')
        num=65
        schn='D2h^19'
        hm_symbol='B 2/m 2/m 2/m'
        hall_num=312
    case('-C 2 2c')
        num=66
        schn='D2h^20'
        hm_symbol='C 2/c 2/c 2/m'
        hall_num=313
    case('-A 2a 2')
        num=66
        schn='D2h^20'
        hm_symbol='A 2/m 2/a 2/a'
        hall_num=314
    case('-B 2b 2b')
        num=66
        schn='D2h^20'
        hm_symbol='B 2/b 2/m 2/b'
        hall_num=315
    case('-C 2b 2')
        num=67
        schn='D2h^21'
        hm_symbol='C 2/m 2/m 2/e'
        hall_num=316
    case('-C 2b 2b')
        num=67
        schn='D2h^21'
        hm_symbol='C 2/m 2/m 2/e'
        hall_num=317
    case('-A 2c 2c')
        num=67
        schn='D2h^21'
        hm_symbol='A 2/e 2/m 2/m'
        hall_num=318
    case('-A 2 2c')
        num=67
        schn='D2h^21'
        hm_symbol='A 2/e 2/m 2/m'
        hall_num=319
    case('-B 2 2c')
        num=67
        schn='D2h^21'
        hm_symbol='B 2/m 2/e 2/m'
        hall_num=320
    case('-B 2c 2')
        num=67
        schn='D2h^21'
        hm_symbol='B 2/m 2/e 2/m'
        hall_num=321
    case('C 2 2 -1bc')
        num=68
        schn='D2h^22'
        hm_symbol='C 2/c 2/c 2/e'
        hall_num=322
    case('-C 2b 2bc')
        num=68
        schn='D2h^22'
        hm_symbol='C 2/c 2/c 2/e'
        hall_num=323
    case('-C 2b 2c')
        num=68
        schn='D2h^22'
        hm_symbol='C 2/c 2/c 2/e'
        hall_num=325
    case('A 2 2 -1ac')
        num=68
        schn='D2h^22'
        hm_symbol='A 2/e 2/a 2/a'
        hall_num=326
    case('-A 2a 2c')
        num=68
        schn='D2h^22'
        hm_symbol='A 2/e 2/a 2/a'
        hall_num=327
    case('-A 2ac 2c')
        num=68
        schn='D2h^22'
        hm_symbol='A 2/e 2/a 2/a'
        hall_num=329
    case('B 2 2 -1bc')
        num=68
        schn='D2h^22'
        hm_symbol='B 2/b 2/e 2/b'
        hall_num=330
    case('-B 2bc 2b')
        num=68
        schn='D2h^22'
        hm_symbol='B 2/b 2/e 2/b'
        hall_num=331
    case('-B 2b 2bc')
        num=68
        schn='D2h^22'
        hm_symbol='B 2/b 2/e 2/b'
        hall_num=333
    case('-F 2 2')
        num=69
        schn='D2h^23'
        hm_symbol='F 2/m 2/m 2/m'
        hall_num=334
    case('F 2 2 -1d')
        num=70
        schn='D2h^24'
        hm_symbol='F 2/d 2/d 2/d'
        hall_num=335
    case('-F 2uv 2vw')
        num=70
        schn='D2h^24'
        hm_symbol='F 2/d 2/d 2/d'
        hall_num=336
    case('-I 2 2')
        num=71
        schn='D2h^25'
        hm_symbol='I 2/m 2/m 2/m'
        hall_num=337
    case('-I 2 2c')
        num=72
        schn='D2h^26'
        hm_symbol='I 2/b 2/a 2/m'
        hall_num=338
    case('-I 2a 2')
        num=72
        schn='D2h^26'
        hm_symbol='I 2/m 2/c 2/b'
        hall_num=339
    case('-I 2b 2b')
        num=72
        schn='D2h^26'
        hm_symbol='I 2/c 2/m 2/a'
        hall_num=340
    case('-I 2b 2c')
        num=73
        schn='D2h^27'
        hm_symbol='I 2/b 2/c 2/a'
        hall_num=341
    case('-I 2a 2b')
        num=73
        schn='D2h^27'
        hm_symbol='I 2/c 2/a 2/b'
        hall_num=342
    case('-I 2b 2')
        num=74
        schn='D2h^28'
        hm_symbol='I 2/m 2/m 2/a'
        hall_num=343
    case('-I 2a 2a')
        num=74
        schn='D2h^28'
        hm_symbol='I 2/m 2/m 2/b'
        hall_num=344
    case('-I 2c 2c')
        num=74
        schn='D2h^28'
        hm_symbol='I 2/b 2/m 2/m'
        hall_num=345
    case('-I 2 2b')
        num=74
        schn='D2h^28'
        hm_symbol='I 2/c 2/m 2/m'
        hall_num=346
    case('-I 2 2a')
        num=74
        schn='D2h^28'
        hm_symbol='I 2/m 2/c 2/m'
        hall_num=347
    case('-I 2c 2')
        num=74
        schn='D2h^28'
        hm_symbol='I 2/m 2/a 2/m'
        hall_num=348
    case('P 4')
        num=75
        schn='C4^1'
        hm_symbol='P 4'
        hall_num=349
    case('P 4w')
        num=76
        schn='C4^2'
        hm_symbol='P 4_1'
        hall_num=350
    case('P 4c')
        num=77
        schn='C4^3'
        hm_symbol='P 4_2'
        hall_num=351
    case('P 4cw')
        num=78
        schn='C4^4'
        hm_symbol='P 4_3'
        hall_num=352
    case('I 4')
        num=79
        schn='C4^5'
        hm_symbol='I 4'
        hall_num=353
    case('I 4bw')
        num=80
        schn='C4^6'
        hm_symbol='I 4_1'
        hall_num=354
    case('P -4')
        num=81
        schn='S4^1'
        hm_symbol='P -4'
        hall_num=355
    case('I -4')
        num=82
        schn='S4^2'
        hm_symbol='I -4'
        hall_num=356
    case('-P 4')
        num=83
        schn='C4h^1'
        hm_symbol='P 4/m'
        hall_num=357
    case('-P 4c')
        num=84
        schn='C4h^2'
        hm_symbol='P 4_2/m'
        hall_num=358
    case('P 4ab -1ab')
        num=85
        schn='C4h^3'
        hm_symbol='P 4/n'
        hall_num=359
    case('-P 4a')
        num=85
        schn='C4h^3'
        hm_symbol='P 4/n'
        hall_num=360
    case('P 4n -1n')
        num=86
        schn='C4h^4'
        hm_symbol='P 4_2/n'
        hall_num=361
    case('-P 4bc')
        num=86
        schn='C4h^4'
        hm_symbol='P 4_2/n'
        hall_num=362
    case('-I 4')
        num=87
        schn='C4h^5'
        hm_symbol='I 4/m'
        hall_num=363
    case('I 4bw -1bw')
        num=88
        schn='C4h^6'
        hm_symbol='I 4_1/a'
        hall_num=364
    case('-I 4ad')
        num=88
        schn='C4h^6'
        hm_symbol='I 4_1/a'
        hall_num=365
    case('P 4 2')
        num=89
        schn='D4^1'
        hm_symbol='P 4 2 2'
        hall_num=366
    case('P 4ab 2ab')
        num=90
        schn='D4^2'
        hm_symbol='P 4 2_1 2'
        hall_num=367
    case('P 4w 2c')
        num=91
        schn='D4^3'
        hm_symbol='P 4_1 2 2'
        hall_num=368
    case('P 4abw 2nw')
        num=92
        schn='D4^4'
        hm_symbol='P 4_1 2_1 2'
        hall_num=369
    case('P 4c 2')
        num=93
        schn='D4^5'
        hm_symbol='P 4_2 2 2'
        hall_num=370
    case('P 4n 2n')
        num=94
        schn='D4^6'
        hm_symbol='P 4_2 2_1 2'
        hall_num=371
    case('P 4cw 2c')
        num=95
        schn='D4^7'
        hm_symbol='P 4_3 2 2'
        hall_num=372
    case('P 4nw 2abw')
        num=96
        schn='D4^8'
        hm_symbol='P 4_3 2_1 2'
        hall_num=373
    case('I 4 2')
        num=97
        schn='D4^9'
        hm_symbol='I 4 2 2'
        hall_num=374
    case('I 4bw 2bw')
        num=98
        schn='D4^10'
        hm_symbol='I 4_1 2 2'
        hall_num=375
    case('P 4 -2')
        num=99
        schn='C4v^1'
        hm_symbol='P 4 m m'
        hall_num=376
    case('P 4 -2ab')
        num=100
        schn='C4v^2'
        hm_symbol='P 4 b m'
        hall_num=377
    case('P 4c -2c')
        num=101
        schn='C4v^3'
        hm_symbol='P 4_2 c m'
        hall_num=378
    case('P 4n -2n')
        num=102
        schn='C4v^4'
        hm_symbol='P 4_2 n m'
        hall_num=379
    case('P 4 -2c')
        num=103
        schn='C4v^5'
        hm_symbol='P 4 c c'
        hall_num=380
    case('P 4 -2n')
        num=104
        schn='C4v^6'
        hm_symbol='P 4 n c'
        hall_num=381
    case('P 4c -2')
        num=105
        schn='C4v^7'
        hm_symbol='P 4_2 m c'
        hall_num=382
    case('P 4c -2ab')
        num=106
        schn='C4v^8'
        hm_symbol='P 4_2 b c'
        hall_num=383
    case('I 4 -2')
        num=107
        schn='C4v^9'
        hm_symbol='I 4 m m'
        hall_num=384
    case('I 4 -2c')
        num=108
        schn='C4v^10'
        hm_symbol='I 4 c m'
        hall_num=385
    case('I 4bw -2')
        num=109
        schn='C4v^11'
        hm_symbol='I 4_1 m d'
        hall_num=386
    case('I 4bw -2c')
        num=110
        schn='C4v^12'
        hm_symbol='I 4_1 c d'
        hall_num=387
    case('P -4 2')
        num=111
        schn='D2d^1'
        hm_symbol='P -4 2 m'
        hall_num=388
    case('P -4 2c')
        num=112
        schn='D2d^2'
        hm_symbol='P -4 2 c'
        hall_num=389
    case('P -4 2ab')
        num=113
        schn='D2d^3'
        hm_symbol='P -4 2_1 m'
        hall_num=390
    case('P -4 2n')
        num=114
        schn='D2d^4'
        hm_symbol='P -4 2_1 c'
        hall_num=391
    case('P -4 -2')
        num=115
        schn='D2d^5'
        hm_symbol='P -4 m 2'
        hall_num=392
    case('P -4 -2c')
        num=116
        schn='D2d^6'
        hm_symbol='P -4 c 2'
        hall_num=393
    case('P -4 -2ab')
        num=117
        schn='D2d^7'
        hm_symbol='P -4 b 2'
        hall_num=394
    case('P -4 -2n')
        num=118
        schn='D2d^8'
        hm_symbol='P -4 n 2'
        hall_num=395
    case('I -4 -2')
        num=119
        schn='D2d^9'
        hm_symbol='I -4 m 2'
        hall_num=396
    case('I -4 -2c')
        num=120
        schn='D2d^10'
        hm_symbol='I -4 c 2'
        hall_num=397
    case('I -4 2')
        num=121
        schn='D2d^11'
        hm_symbol='I -4 2 m'
        hall_num=398
    case('I -4 2bw')
        num=122
        schn='D2d^12'
        hm_symbol='I -4 2 d'
        hall_num=399
    case('-P 4 2')
        num=123
        schn='D4h^1'
        hm_symbol='P 4/m 2/m 2/m'
        hall_num=400
    case('-P 4 2c')
        num=124
        schn='D4h^2'
        hm_symbol='P 4/m 2/c 2/c'
        hall_num=401
    case('P 4 2 -1ab')
        num=125
        schn='D4h^3'
        hm_symbol='P 4/n 2/b 2/m'
        hall_num=402
    case('-P 4a 2b')
        num=125
        schn='D4h^3'
        hm_symbol='P 4/n 2/b 2/m'
        hall_num=403
    case('P 4 2 -1n')
        num=126
        schn='D4h^4'
        hm_symbol='P 4/n 2/n 2/c'
        hall_num=404
    case('-P 4a 2bc')
        num=126
        schn='D4h^4'
        hm_symbol='P 4/n 2/n 2/c'
        hall_num=405
    case('-P 4 2ab')
        num=127
        schn='D4h^5'
        hm_symbol='P 4/m 2_1/b m'
        hall_num=406
    case('-P 4 2n')
        num=128
        schn='D4h^6'
        hm_symbol='P 4/m 2_1/n c'
        hall_num=407
    case('P 4ab 2ab -1ab')
        num=129
        schn='D4h^7'
        hm_symbol='P 4/n 2_1/m m'
        hall_num=408
    case('-P 4a 2a')
        num=129
        schn='D4h^7'
        hm_symbol='P 4/n 2_1/m m'
        hall_num=409
    case('P 4ab 2n -1ab')
        num=130
        schn='D4h^8'
        hm_symbol='P 4/n 2_1/c c'
        hall_num=410
    case('-P 4a 2ac')
        num=130
        schn='D4h^8'
        hm_symbol='P 4/n 2_1/c c'
        hall_num=411
    case('-P 4c 2')
        num=131
        schn='D4h^9'
        hm_symbol='P 4_2/m 2/m 2/c'
        hall_num=412
    case('-P 4c 2c')
        num=132
        schn='D4h^10'
        hm_symbol='P 4_2/m 2/c 2/m'
        hall_num=413
    case('P 4n 2c -1n')
        num=133
        schn='D4h^11'
        hm_symbol='P 4_2/n 2/b 2/c'
        hall_num=414
    case('-P 4ac 2b')
        num=133
        schn='D4h^11'
        hm_symbol='P 4_2/n 2/b 2/c'
        hall_num=415
    case('P 4n 2 -1n')
        num=134
        schn='D4h^12'
        hm_symbol='P 4_2/n 2/n 2/m'
        hall_num=416
    case('-P 4ac 2bc')
        num=134
        schn='D4h^12'
        hm_symbol='P 4_2/n 2/n 2/m'
        hall_num=417
    case('-P 4c 2ab')
        num=135
        schn='D4h^13'
        hm_symbol='P 4_2/m 2_1/b 2/c'
        hall_num=418
    case('-P 4n 2n')
        num=136
        schn='D4h^14'
        hm_symbol='P 4_2/m 2_1/n 2/m'
        hall_num=419
    case('P 4n 2n -1n')
        num=137
        schn='D4h^15'
        hm_symbol='P 4_2/n 2_1/m 2/c'
        hall_num=420
    case('-P 4ac 2a')
        num=137
        schn='D4h^15'
        hm_symbol='P 4_2/n 2_1/m 2/c'
        hall_num=421
    case('P 4n 2ab -1n')
        num=138
        schn='D4h^16'
        hm_symbol='P 4_2/n 2_1/c 2/m'
        hall_num=422
    case('-P 4ac 2ac')
        num=138
        schn='D4h^16'
        hm_symbol='P 4_2/n 2_1/c 2/m'
        hall_num=423
    case('-I 4 2')
        num=139
        schn='D4h^17'
        hm_symbol='I 4/m 2/m 2/m'
        hall_num=424
    case('-I 4 2c')
        num=140
        schn='D4h^18'
        hm_symbol='I 4/m 2/c 2/m'
        hall_num=425
    case('I 4bw 2bw -1bw')
        num=141
        schn='D4h^19'
        hm_symbol='I 4_1/a 2/m 2/d'
        hall_num=426
    case('-I 4bd 2')
        num=141
        schn='D4h^19'
        hm_symbol='I 4_1/a 2/m 2/d'
        hall_num=427
    case('I 4bw 2aw -1bw')
        num=142
        schn='D4h^20'
        hm_symbol='I 4_1/a 2/c 2/d'
        hall_num=428
    case('-I 4bd 2c')
        num=142
        schn='D4h^20'
        hm_symbol='I 4_1/a 2/c 2/d'
        hall_num=429
    case('P 3')
        num=143
        schn='C3^1'
        hm_symbol='P 3'
        hall_num=430
    case('P 31')
        num=144
        schn='C3^2'
        hm_symbol='P 3_1'
        hall_num=431
    case('P 32')
        num=145
        schn='C3^3'
        hm_symbol='P 3_2'
        hall_num=432
    case('R 3')
        num=146
        schn='C3^4'
        hm_symbol='R 3'
        hall_num=433
    case('P 3*')
        num=146
        schn='C3^4'
        hm_symbol='R 3'
        hall_num=434
    case('-P 3')
        num=147
        schn='C3i^1'
        hm_symbol='P -3'
        hall_num=435
    case('-R 3')
        num=148
        schn='C3i^2'
        hm_symbol='R -3'
        hall_num=436
    case('-P 3*')
        num=148
        schn='C3i^2'
        hm_symbol='R -3'
        hall_num=437
    case('P 3 2')
        num=149
        schn='D3^1'
        hm_symbol='P 3 1 2'
        hall_num=438
    case('P 3 2"')
        num=150
        schn='D3^2'
        hm_symbol='P 3 2 1'
        hall_num=439
    case('P 31 2c (0 0 1)')
        num=151
        schn='D3^3'
        hm_symbol='P 3_1 1 2'
        hall_num=440
    case('P 31 2"')
        num=152
        schn='D3^4'
        hm_symbol='P 3_1 2 1'
        hall_num=441
    case('P 32 2c (0 0 -1)')
        num=153
        schn='D3^5'
        hm_symbol='P 3_2 1 2'
        hall_num=442
    case('P 32 2"')
        num=154
        schn='D3^6'
        hm_symbol='P 3_2 2 1'
        hall_num=443
    case('R 3 2"')
        num=155
        schn='D3^7'
        hm_symbol='R 3 2'
        hall_num=444
    case('P 3* 2')
        num=155
        schn='D3^7'
        hm_symbol='R 3 2'
        hall_num=445
    case('P 3 -2"')
        num=156
        schn='C3v^1'
        hm_symbol='P 3 m 1'
        hall_num=446
    case('P 3 -2')
        num=157
        schn='C3v^2'
        hm_symbol='P 3 1 m'
        hall_num=447
    case('P 3 -2"c')
        num=158
        schn='C3v^3'
        hm_symbol='P 3 c 1'
        hall_num=448
    case('P 3 -2c')
        num=159
        schn='C3v^4'
        hm_symbol='P 3 1 c'
        hall_num=449
    case('R 3 -2"')
        num=160
        schn='C3v^5'
        hm_symbol='R 3 m'
        hall_num=450
    case('P 3* -2')
        num=160
        schn='C3v^5'
        hm_symbol='R 3 m'
        hall_num=451
    case('R 3 -2"c')
        num=161
        schn='C3v^6'
        hm_symbol='R 3 c'
        hall_num=452
    case('P 3* -2n')
        num=161
        schn='C3v^6'
        hm_symbol='R 3 c'
        hall_num=453
    case('-P 3 2')
        num=162
        schn='D3d^1'
        hm_symbol='P -3 1 2/m'
        hall_num=454
    case('-P 3 2c')
        num=163
        schn='D3d^2'
        hm_symbol='P -3 1 2/c'
        hall_num=455
    case('-P 3 2"')
        num=164
        schn='D3d^3'
        hm_symbol='P -3 2/m 1'
        hall_num=456
    case('-P 3 2"c')
        num=165
        schn='D3d^4'
        hm_symbol='P -3 2/c 1'
        hall_num=457
    case('-R 3 2"')
        num=166
        schn='D3d^5'
        hm_symbol='R -3 2/m'
        hall_num=458
    case('-P 3* 2')
        num=166
        schn='D3d^5'
        hm_symbol='R -3 2/m'
        hall_num=459
    case('-R 3 2"c')
        num=167
        schn='D3d^6'
        hm_symbol='R -3 2/c'
        hall_num=460
    case('-P 3* 2n')
        num=167
        schn='D3d^6'
        hm_symbol='R -3 2/c'
        hall_num=461
    case('P 6')
        num=168
        schn='C6^1'
        hm_symbol='P 6'
        hall_num=462
    case('P 61')
        num=169
        schn='C6^2'
        hm_symbol='P 6_1'
        hall_num=463
    case('P 65')
        num=170
        schn='C6^3'
        hm_symbol='P 6_5'
        hall_num=464
    case('P 62')
        num=171
        schn='C6^4'
        hm_symbol='P 6_2'
        hall_num=465
    case('P 64')
        num=172
        schn='C6^5'
        hm_symbol='P 6_4'
        hall_num=466
    case('P 6c')
        num=173
        schn='C6^6'
        hm_symbol='P 6_3'
        hall_num=467
    case('P -6')
        num=174
        schn='C3h^1'
        hm_symbol='P -6'
        hall_num=468
    case('-P 6')
        num=175
        schn='C6h^1'
        hm_symbol='P 6/m'
        hall_num=469
    case('-P 6c')
        num=176
        schn='C6h^2'
        hm_symbol='P 6_3/m'
        hall_num=470
    case('P 6 2')
        num=177
        schn='D6^1'
        hm_symbol='P 6 2 2'
        hall_num=471
    case('P 61 2 (0 0 -1)')
        num=178
        schn='D6^2'
        hm_symbol='P 6_1 2 2'
        hall_num=472
    case('P 65 2 (0 0 1)')
        num=179
        schn='D6^3'
        hm_symbol='P 6_5 2 2'
        hall_num=473
    case('P 62 2c (0 0 1)')
        num=180
        schn='D6^4'
        hm_symbol='P 6_2 2 2'
        hall_num=474
    case('P 64 2c (0 0 -1)')
        num=181
        schn='D6^5'
        hm_symbol='P 6_4 2 2'
        hall_num=475
    case('P 6c 2c')
        num=182
        schn='D6^6'
        hm_symbol='P 6_3 2 2'
        hall_num=476
    case('P 6 -2')
        num=183
        schn='C6v^1'
        hm_symbol='P 6 m m'
        hall_num=477
    case('P 6 -2c')
        num=184
        schn='C6v^2'
        hm_symbol='P 6 c c'
        hall_num=478
    case('P 6c -2')
        num=185
        schn='C6v^3'
        hm_symbol='P 6_3 c m'
        hall_num=479
    case('P 6c -2c')
        num=186
        schn='C6v^4'
        hm_symbol='P 6_3 m c'
        hall_num=480
    case('P -6 2')
        num=187
        schn='D3h^1'
        hm_symbol='P -6 m 2'
        hall_num=481
    case('P -6c 2')
        num=188
        schn='D3h^2'
        hm_symbol='P -6 c 2'
        hall_num=482
    case('P -6 -2')
        num=189
        schn='D3h^3'
        hm_symbol='P -6 2 m'
        hall_num=483
    case('P -6c -2c')
        num=190
        schn='D3h^4'
        hm_symbol='P -6 2 c'
        hall_num=484
    case('-P 6 2')
        num=191
        schn='D6h^1'
        hm_symbol='P 6/m 2/m 2/m'
        hall_num=485
    case('-P 6 2c')
        num=192
        schn='D6h^2'
        hm_symbol='P 6/m 2/c 2/c'
        hall_num=486
    case('-P 6c 2')
        num=193
        schn='D6h^3'
        hm_symbol='P 6_3/m 2/c 2/m'
        hall_num=487
    case('-P 6c 2c')
        num=194
        schn='D6h^4'
        hm_symbol='P 6_3/m 2/m 2/c'
        hall_num=488
    case('P 2 2 3')
        num=195
        schn='T^1'
        hm_symbol='P 2 3'
        hall_num=489
    case('F 2 2 3')
        num=196
        schn='T^2'
        hm_symbol='F 2 3'
        hall_num=490
    case('I 2 2 3')
        num=197
        schn='T^3'
        hm_symbol='I 2 3'
        hall_num=491
    case('P 2ac 2ab 3')
        num=198
        schn='T^4'
        hm_symbol='P 2_1 3'
        hall_num=492
    case('I 2b 2c 3')
        num=199
        schn='T^5'
        hm_symbol='I 2_1 3'
        hall_num=493
    case('-P 2 2 3')
        num=200
        schn='Th^1'
        hm_symbol='P 2/m -3'
        hall_num=494
    case('P 2 2 3 -1n')
        num=201
        schn='Th^2'
        hm_symbol='P 2/n -3'
        hall_num=495
    case('-P 2ab 2bc 3')
        num=201
        schn='Th^2'
        hm_symbol='P 2/n -3'
        hall_num=496
    case('-F 2 2 3')
        num=202
        schn='Th^3'
        hm_symbol='F 2/m -3'
        hall_num=497
    case('F 2 2 3 -1d')
        num=203
        schn='Th^4'
        hm_symbol='F 2/d -3'
        hall_num=498
    case('-F 2uv 2vw 3')
        num=203
        schn='Th^4'
        hm_symbol='F 2/d -3'
        hall_num=499
    case('-I 2 2 3')
        num=204
        schn='Th^5'
        hm_symbol='I 2/m -3'
        hall_num=500
    case('-P 2ac 2ab 3')
        num=205
        schn='Th^6'
        hm_symbol='P 2_1/a -3'
        hall_num=501
    case('-I 2b 2c 3')
        num=206
        schn='Th^7'
        hm_symbol='I 2_1/a -3'
        hall_num=502
    case('P 4 2 3')
        num=207
        schn='O^1'
        hm_symbol='P 4 3 2'
        hall_num=503
    case('P 4n 2 3')
        num=208
        schn='O^2'
        hm_symbol='P 4_2 3 2'
        hall_num=504
    case('F 4 2 3')
        num=209
        schn='O^3'
        hm_symbol='F 4 3 2'
        hall_num=505
    case('F 4d 2 3')
        num=210
        schn='O^4'
        hm_symbol='F 4_1 3 2'
        hall_num=506
    case('I 4 2 3')
        num=211
        schn='O^5'
        hm_symbol='I 4 3 2'
        hall_num=507
    case('P 4acd 2ab 3')
        num=212
        schn='O^6'
        hm_symbol='P 4_3 3 2'
        hall_num=508
    case('P 4bd 2ab 3')
        num=213
        schn='O^7'
        hm_symbol='P 4_1 3 2'
        hall_num=509
    case('I 4bd 2c 3')
        num=214
        schn='O^8'
        hm_symbol='I 4_1 3 2'
        hall_num=510
    case('P -4 2 3')
        num=215
        schn='Td^1'
        hm_symbol='P -4 3 m'
        hall_num=511
    case('F -4 2 3')
        num=216
        schn='Td^2'
        hm_symbol='F -4 3 m'
        hall_num=512
    case('I -4 2 3')
        num=217
        schn='Td^3'
        hm_symbol='I -4 3 m'
        hall_num=513
    case('P -4n 2 3')
        num=218
        schn='Td^4'
        hm_symbol='P -4 3 n'
        hall_num=514
    case('F -4c 2 3')
        num=219
        schn='Td^5'
        hm_symbol='F -4 3 c'
        hall_num=515
    case('I -4bd 2c 3')
        num=220
        schn='Td^6'
        hm_symbol='I -4 3 d'
        hall_num=516
    case('-P 4 2 3')
        num=221
        schn='Oh^1'
        hm_symbol='P 4/m -3 2/m'
        hall_num=517
    case('P 4 2 3 -1n')
        num=222
        schn='Oh^2'
        hm_symbol='P 4/n -3 2/n'
        hall_num=518
    case('-P 4a 2bc 3')
        num=222
        schn='Oh^2'
        hm_symbol='P 4/n -3 2/n'
        hall_num=519
    case('-P 4n 2 3')
        num=223
        schn='Oh^3'
        hm_symbol='P 4_2/m -3 2/n'
        hall_num=520
    case('P 4n 2 3 -1n')
        num=224
        schn='Oh^4'
        hm_symbol='P 4_2/n -3 2/m'
        hall_num=521
    case('-P 4bc 2bc 3')
        num=224
        schn='Oh^4'
        hm_symbol='P 4_2/n -3 2/m'
        hall_num=522
    case('-F 4 2 3')
        num=225
        schn='Oh^5'
        hm_symbol='F 4/m -3 2/m'
        hall_num=523
    case('-F 4c 2 3')
        num=226
        schn='Oh^6'
        hm_symbol='F 4/m -3 2/c'
        hall_num=524
    case('F 4d 2 3 -1d')
        num=227
        schn='Oh^7'
        hm_symbol='F 4_1/d -3 2/m'
        hall_num=525
    case('-F 4vw 2vw 3')
        num=227
        schn='Oh^7'
        hm_symbol='F 4_1/d -3 2/m'
        hall_num=526
    case('F 4d 2 3 -1cd')
        num=228
        schn='Oh^8'
        hm_symbol='F 4_1/d -3 2/c'
        hall_num=527
    case('-F 4cvw 2vw 3')
        num=228
        schn='Oh^8'
        hm_symbol='F 4_1/d -3 2/c'
        hall_num=528
    case('-I 4 2 3')
        num=229
        schn='Oh^9'
        hm_symbol='I 4/m -3 2/m'
        hall_num=529
    case('-I 4bd 2c 3')
        num=230
        schn='Oh^10'
        hm_symbol='I 4_1/a -3 2/d'
        hall_num=530


    case default
       write(*,*)
       write(*,*) '"Error(sgsymb): Hall symbol"'
       write(*,*)
       stop
    end select
    return
  end subroutine sgsymb
  !EOC
  ! still need to rebuild 
  ! many error
  subroutine getwyckoff(hall_num, w, steer)
    integer, intent(in) :: hall_num
    real(dp), intent(inout) :: w(:,:)
    integer, intent(inout) :: steer(:)
    
    steer(20)=0
    w(:,:) = 0.0
    select case(hall_num)


    case(6)
       w(2,2) = 0.5
    case(7)
       w(2,3) = 0.5
    case(8)
       w(2,1) = 0.5
    case(21)
       w(2,3) = 0.5
    case(22)
       w(2,1) = 0.5
       w(2,3) = 0.5
    case(23)
       w(2,1) = 0.5
    case(24)
       w(2,1) = 0.5
    case(25)
       w(2,1) = 0.5
       w(2,2) = 0.5
    case(26)
       w(2,2) = 0.5
    case(27)
       w(2,2) = 0.5
    case(28)
       w(2,2) = 0.5
       w(2,3) = 0.5
    case(29)
       w(2,3) = 0.5
    case(39)
       w(2,3) = 0.5
    case(40)
       w(2,1) = 0.5
       w(2,3) = 0.5
    case(41)
       w(2,1) = 0.5
    case(42)
       w(2,1) = 0.5
    case(43)
       w(2,2) = 0.5
       w(2,3) = 0.5
    case(44)
       w(2,3) = 0.5
    case(45)
       w(2,1) = 0.5
    case(46)
       w(2,2) = 0.5
       w(2,3) = 0.5
    case(47)
       w(2,2) = 0.5
    case(48)
       w(2,2) = 0.5
    case(49)
       w(2,1) = 0.5
       w(2,3) = 0.5
    case(50)
       w(2,1) = 0.5
    case(51)
       w(2,2) = 0.5
    case(52)
       w(2,2) = 0.5
       w(2,3) = 0.5
    case(53)
       w(2,3) = 0.5
    case(54)
       w(2,3) = 0.5
    case(55)
       w(2,2) = 0.5
       w(2,3) = 0.5
    case(56)
       w(2,2) = 0.5
    case(60)
       w(3,2) = 0.5
       w(4,2) = 0.5
    case(61)
       w(3,3) = 0.5
       w(4,3) = 0.5
    case(62)
       w(3,1) = 0.5
       w(4,1) = 0.5
    case(72)
       w(3,3) = 0.5
       w(4,3) = 0.5
    case(73)
       w(3,1) = 0.5
       w(3,3) = 0.5
       w(4,1) = 0.5
       w(4,3) = 0.5
    case(74)
       w(3,1) = 0.5
       w(4,1) = 0.5
    case(75)
       w(3,1) = 0.5
       w(4,1) = 0.5
    case(76)
       w(3,1) = 0.5
       w(3,2) = 0.5
       w(4,1) = 0.5
       w(4,2) = 0.5
    case(77)
       w(3,2) = 0.5
       w(4,2) = 0.5
    case(78)
       w(3,2) = 0.5
       w(4,2) = 0.5
    case(79)
       w(3,2) = 0.5
       w(3,3) = 0.5
       w(4,2) = 0.5
       w(4,3) = 0.5
    case(80)
       w(3,3) = 0.5
       w(4,3) = 0.5
    case(81)
       w(3,2) = 0.5
       w(3,3) = 0.5
       w(4,2) = 0.5
       w(4,3) = 0.5
    case(82)
       w(3,1) = 0.5
       w(3,2) = 0.5
       w(3,3) = 0.5
       w(4,1) = 0.5
       w(4,2) = 0.5
       w(4,3) = 0.5
    case(83)
       w(3,1) = 0.5
       w(3,2) = 0.5
       w(4,1) = 0.5
       w(4,2) = 0.5
    case(84)
       w(3,1) = 0.5
       w(3,3) = 0.5
       w(4,1) = 0.5
       w(4,3) = 0.5
    case(85)
       w(3,1) = 0.5
       w(3,2) = 0.5
       w(3,3) = 0.5
       w(4,1) = 0.5
       w(4,2) = 0.5
       w(4,3) = 0.5
    case(86)
       w(3,2) = 0.5
       w(3,3) = 0.5
       w(4,2) = 0.5
       w(4,3) = 0.5
    case(87)
       w(3,1) = 0.5
       w(3,2) = 0.5
       w(4,1) = 0.5
       w(4,2) = 0.5
    case(88)
       w(3,1) = 0.5
       w(3,2) = 0.5
       w(3,3) = 0.5
       w(4,1) = 0.5
       w(4,2) = 0.5
       w(4,3) = 0.5
    case(89)
       w(3,1) = 0.5
       w(3,3) = 0.5
       w(4,1) = 0.5
       w(4,3) = 0.5
    case(90)
       w(3,3) = 0.5
       w(4,3) = 0.5
    case(91)
       w(3,1) = 0.5
       w(3,3) = 0.5
       w(4,1) = 0.5
       w(4,3) = 0.5
    case(92)
       w(3,1) = 0.5
       w(4,1) = 0.5
    case(93)
       w(3,1) = 0.5
       w(4,1) = 0.5
    case(94)
       w(3,2) = 0.5
       w(3,3) = 0.5
       w(4,2) = 0.5
       w(4,3) = 0.5
    case(95)
       w(3,3) = 0.5
       w(4,3) = 0.5
    case(96)
       w(3,1) = 0.5
       w(4,1) = 0.5
    case(97)
       w(3,2) = 0.5
       w(3,3) = 0.5
       w(4,2) = 0.5
       w(4,3) = 0.5
    case(98)
       w(3,2) = 0.5
       w(4,2) = 0.5
    case(99)
       w(3,2) = 0.5
       w(4,2) = 0.5
    case(100)
       w(3,1) = 0.5
       w(3,3) = 0.5
       w(4,1) = 0.5
       w(4,3) = 0.5
    case(101)
       w(3,1) = 0.5
       w(4,1) = 0.5
    case(102)
       w(3,2) = 0.5
       w(4,2) = 0.5
    case(103)
       w(3,2) = 0.5
       w(3,3) = 0.5
       w(4,2) = 0.5
       w(4,3) = 0.5
    case(104)
       w(3,3) = 0.5
       w(4,3) = 0.5
    case(105)
       w(3,3) = 0.5
       w(4,3) = 0.5
    case(106)
       w(3,2) = 0.5
       w(3,3) = 0.5
       w(4,2) = 0.5
       w(4,3) = 0.5
    case(107)
       w(3,2) = 0.5
       w(4,2) = 0.5
    case(109)
       w(2,3) = 0.5
       w(4,3) = 0.5
    case(110)
       w(2,1) = 0.5
       w(3,1) = 0.5
    case(111)
       w(3,2) = 0.5
       w(4,2) = 0.5
    case(112)
       w(3,1) = 0.5
       w(3,2) = 0.5
       w(4,1) = 0.5
       w(4,2) = 0.5
    case(113)
       w(2,2) = 0.5
       w(2,3) = 0.5
       w(4,2) = 0.5
       w(4,3) = 0.5
    case(114)
       w(2,1) = 0.5
       w(2,3) = 0.5
       w(3,1) = 0.5
       w(3,3) = 0.5
    case(115)
       w(2,1) = 0.5
       w(2,3) = 0.5
       w(3,1) = 0.5
       w(3,2) = 0.5
       w(4,2) = 0.5
       w(4,3) = 0.5
    case(116)
       w(2,3) = 0.5
       w(4,3) = 0.5
    case(117)
       w(2,1) = 0.5
       w(3,1) = 0.5
    case(118)
       w(3,2) = 0.5
       w(4,2) = 0.5
    case(124)
       w(2,2) = 0.5
       w(3,3) = 0.5
       w(4,2) = 0.5
       w(4,3) = 0.5
    case(128)
       w(2,3) = 0.5
       w(4,3) = 0.5
    case(129)
       w(2,3) = 0.5
       w(3,3) = 0.5
    case(130)
       w(2,1) = 0.5
       w(3,1) = 0.5
    case(131)
       w(3,1) = 0.5
       w(4,1) = 0.5
    case(132)
       w(3,2) = 0.5
       w(4,2) = 0.5
    case(133)
       w(2,2) = 0.5
       w(4,2) = 0.5
    case(134)
       w(3,3) = 0.5
       w(4,3) = 0.5
    case(135)
       w(2,1) = 0.5
       w(4,1) = 0.5
    case(136)
       w(2,2) = 0.5
       w(3,2) = 0.5
    case(137)
       w(3,1) = 0.5
       w(4,1) = 0.5
    case(138)
       w(3,2) = 0.5
       w(4,2) = 0.5
    case(139)
       w(2,2) = 0.5
       w(4,2) = 0.5
    case(140)
       w(2,3) = 0.5
       w(4,3) = 0.5
    case(141)
       w(2,3) = 0.5
       w(3,3) = 0.5
    case(142)
       w(2,1) = 0.5
       w(3,1) = 0.5
    case(143)
       w(2,3) = 0.5
       w(3,1) = 0.5
       w(3,3) = 0.5
       w(4,1) = 0.5
    case(144)
       w(2,3) = 0.5
       w(3,2) = 0.5
       w(4,2) = 0.5
       w(4,3) = 0.5
    case(145)
       w(2,2) = 0.5
       w(3,1) = 0.5
       w(4,1) = 0.5
       w(4,2) = 0.5
    case(146)
       w(2,1) = 0.5
       w(2,3) = 0.5
       w(3,1) = 0.5
       w(4,3) = 0.5
    case(147)
       w(2,2) = 0.5
       w(2,3) = 0.5
       w(3,3) = 0.5
       w(4,2) = 0.5
    case(148)
       w(2,1) = 0.5
       w(3,1) = 0.5
       w(3,2) = 0.5
       w(4,2) = 0.5
    case(149)
       w(3,2) = 0.5
       w(3,3) = 0.5
       w(4,2) = 0.5
       w(4,3) = 0.5
    case(150)
       w(3,1) = 0.5
       w(3,3) = 0.5
       w(4,1) = 0.5
       w(4,3) = 0.5
    case(151)
       w(2,1) = 0.5
       w(2,3) = 0.5
       w(4,1) = 0.5
       w(4,3) = 0.5
    case(152)
       w(2,1) = 0.5
       w(2,2) = 0.5
       w(4,1) = 0.5
       w(4,2) = 0.5
    case(153)
       w(2,1) = 0.5
       w(2,2) = 0.5
       w(3,1) = 0.5
       w(3,2) = 0.5
    case(154)
       w(2,2) = 0.5
       w(2,3) = 0.5
       w(3,2) = 0.5
       w(3,3) = 0.5
    case(155)
       w(2,1) = 0.5
       w(2,3) = 0.5
       w(4,1) = 0.5
       w(4,3) = 0.5
    case(156)
       w(2,2) = 0.5
       w(2,3) = 0.5
       w(3,2) = 0.5
       w(3,3) = 0.5
    case(157)
       w(2,1) = 0.5
       w(2,2) = 0.5
       w(3,1) = 0.5
       w(3,2) = 0.5
    case(158)
       w(3,1) = 0.5
       w(3,3) = 0.5
       w(4,1) = 0.5
       w(4,3) = 0.5
    case(159)
       w(3,2) = 0.5
       w(3,3) = 0.5
       w(4,2) = 0.5
       w(4,3) = 0.5
    case(160)
       w(2,1) = 0.5
       w(2,2) = 0.5
       w(4,1) = 0.5
       w(4,2) = 0.5
    case(161)
       w(3,1) = 0.5
       w(3,2) = 0.5
       w(4,1) = 0.5
       w(4,2) = 0.5
    case(162)
       w(2,2) = 0.5
       w(2,3) = 0.5
       w(4,2) = 0.5
       w(4,3) = 0.5
    case(163)
       w(2,1) = 0.5
       w(2,3) = 0.5
       w(3,1) = 0.5
       w(3,3) = 0.5
    case(164)
       w(2,3) = 0.5
       w(3,1) = 0.5
       w(3,2) = 0.5
       w(3,3) = 0.5
       w(4,1) = 0.5
       w(4,2) = 0.5
    case(165)
       w(2,3) = 0.5
       w(3,1) = 0.5
       w(3,2) = 0.5
       w(4,1) = 0.5
       w(4,2) = 0.5
       w(4,3) = 0.5
    case(166)
       w(2,2) = 0.5
       w(2,3) = 0.5
       w(3,1) = 0.5
       w(4,1) = 0.5
       w(4,2) = 0.5
       w(4,3) = 0.5
    case(167)
       w(2,1) = 0.5
       w(2,2) = 0.5
       w(2,3) = 0.5
       w(3,1) = 0.5
       w(4,2) = 0.5
       w(4,3) = 0.5
    case(168)
       w(2,1) = 0.5
       w(2,2) = 0.5
       w(2,3) = 0.5
       w(3,1) = 0.5
       w(3,3) = 0.5
       w(4,2) = 0.5
    case(169)
       w(2,1) = 0.5
       w(2,3) = 0.5
       w(3,1) = 0.5
       w(3,2) = 0.5
       w(3,3) = 0.5
       w(4,2) = 0.5
    case(170)
       w(3,1) = 0.5
       w(3,2) = 0.5
       w(3,3) = 0.5
       w(4,1) = 0.5
       w(4,2) = 0.5
       w(4,3) = 0.5
    case(171)
       w(2,1) = 0.5
       w(2,2) = 0.5
       w(2,3) = 0.5
       w(4,1) = 0.5
       w(4,2) = 0.5
       w(4,3) = 0.5
    case(172)
       w(2,1) = 0.5
       w(2,2) = 0.5
       w(2,3) = 0.5
       w(3,1) = 0.5
       w(3,2) = 0.5
       w(3,3) = 0.5
    case(176)
       w(2,3) = 0.5
       w(4,3) = 0.5
    case(177)
       w(2,3) = 0.5
       w(3,3) = 0.5
    case(178)
       w(2,1) = 0.5
       w(3,1) = 0.5
    case(179)
       w(3,1) = 0.5
       w(4,1) = 0.5
    case(180)
       w(3,2) = 0.5
       w(4,2) = 0.5
    case(181)
       w(2,2) = 0.5
       w(4,2) = 0.5
    case(182)
       w(3,3) = 0.5
       w(4,3) = 0.5
    case(183)
       w(2,1) = 0.5
       w(4,1) = 0.5
    case(184)
       w(2,2) = 0.5
       w(3,2) = 0.5
    case(191)
       w(3,3) = 0.5
       w(4,3) = 0.5
    case(192)
       w(3,3) = 0.5
       w(4,3) = 0.5
    case(193)
       w(2,3) = 0.5
       w(4,3) = 0.5
    case(194)
       w(2,2) = 0.5
       w(4,2) = 0.5
    case(195)
       w(2,2) = 0.5
       w(3,2) = 0.5
    case(196)
       w(2,3) = 0.5
       w(3,3) = 0.5
    case(197)
       w(3,1) = 0.5
       w(4,1) = 0.5
    case(198)
       w(3,2) = 0.5
       w(4,2) = 0.5
    case(199)
       w(2,2) = 0.5
       w(4,2) = 0.5
    case(200)
       w(2,3) = 0.5
       w(4,3) = 0.5
    case(201)
       w(2,3) = 0.5
       w(3,3) = 0.5
    case(202)
       w(2,1) = 0.5
       w(3,1) = 0.5
    case(203)
       w(3,1) = 0.5
       w(3,3) = 0.5
       w(4,1) = 0.5
       w(4,3) = 0.5
    case(204)
       w(3,2) = 0.5
       w(3,3) = 0.5
       w(4,2) = 0.5
       w(4,3) = 0.5
    case(205)
       w(2,2) = 0.5
       w(2,3) = 0.5
       w(4,2) = 0.5
       w(4,3) = 0.5
    case(206)
       w(2,2) = 0.5
       w(2,3) = 0.5
       w(4,2) = 0.5
       w(4,3) = 0.5
    case(207)
       w(2,2) = 0.5
       w(2,3) = 0.5
       w(3,2) = 0.5
       w(3,3) = 0.5
    case(208)
       w(2,1) = 0.5
       w(2,3) = 0.5
       w(3,1) = 0.5
       w(3,3) = 0.5
    case(212)
       w(3,1) = 0.25
       w(3,2) = 0.75
       w(3,3) = 0.75
       w(4,1) = 0.25
       w(4,2) = 0.75
       w(4,3) = 0.75
    case(213)
       w(2,1) = 0.75
       w(2,2) = 0.75
       w(2,3) = 0.25
       w(4,1) = 0.75
       w(4,2) = 0.25
       w(4,3) = 0.75
    case(214)
       w(2,1) = 0.75
       w(2,2) = 0.75
       w(2,3) = 0.25
       w(3,1) = 0.25
       w(3,2) = 0.75
       w(3,3) = 0.75
       w(4,1) = 0.5
       w(4,2) = 0.5
    case(218)
       w(3,3) = 0.5
       w(4,3) = 0.5
    case(219)
       w(2,1) = 0.5
       w(4,1) = 0.5
    case(220)
       w(2,2) = 0.5
       w(3,2) = 0.5
    case(221)
       w(3,1) = 0.5
       w(4,1) = 0.5
    case(222)
       w(3,2) = 0.5
       w(4,2) = 0.5
    case(223)
       w(2,2) = 0.5
       w(4,2) = 0.5
    case(224)
       w(2,3) = 0.5
       w(4,3) = 0.5
    case(225)
       w(2,3) = 0.5
       w(3,3) = 0.5
    case(226)
       w(2,1) = 0.5
       w(3,1) = 0.5
    case(228)
       w(5,1) = 0.5
       w(5,2) = 0.5
       w(5,3) = 0.5
       w(6,1) = 0.5
       w(6,2) = 0.5
       w(6,3) = 0.5
       w(7,1) = 0.5
       w(7,2) = 0.5
       w(7,3) = 0.5
       w(8,1) = 0.5
       w(8,2) = 0.5
       w(8,3) = 0.5
    case(229)
       w(3,1) = 0.5
       w(3,2) = 0.5
       w(4,1) = 0.5
       w(4,2) = 0.5
       w(5,2) = 0.5
       w(5,3) = 0.5
       w(6,2) = 0.5
       w(6,3) = 0.5
       w(7,1) = 0.5
       w(7,3) = 0.5
       w(8,1) = 0.5
       w(8,3) = 0.5
    case(230)
       w(5,3) = 0.5
       w(6,3) = 0.5
       w(7,3) = 0.5
       w(8,3) = 0.5
    case(231)
       w(3,1) = 0.5
       w(4,1) = 0.5
       w(7,1) = 0.5
       w(8,1) = 0.5
    case(232)
       w(3,2) = 0.5
       w(4,2) = 0.5
       w(5,2) = 0.5
       w(6,2) = 0.5
    case(233)
       w(5,1) = 0.5
       w(5,2) = 0.5
       w(6,1) = 0.5
       w(6,2) = 0.5
       w(7,1) = 0.5
       w(7,2) = 0.5
       w(8,1) = 0.5
       w(8,2) = 0.5
    case(234)
       w(3,1) = 0.5
       w(3,2) = 0.5
       w(4,1) = 0.5
       w(4,2) = 0.5
       w(5,2) = 0.5
       w(6,2) = 0.5
       w(7,1) = 0.5
       w(8,1) = 0.5
    case(235)
       w(5,2) = 0.5
       w(5,3) = 0.5
       w(6,2) = 0.5
       w(6,3) = 0.5
       w(7,2) = 0.5
       w(7,3) = 0.5
       w(8,2) = 0.5
       w(8,3) = 0.5
    case(236)
       w(3,2) = 0.5
       w(4,2) = 0.5
       w(5,2) = 0.5
       w(5,3) = 0.5
       w(6,2) = 0.5
       w(6,3) = 0.5
       w(7,3) = 0.5
       w(8,3) = 0.5
    case(237)
       w(5,1) = 0.5
       w(5,3) = 0.5
       w(6,1) = 0.5
       w(6,3) = 0.5
       w(7,1) = 0.5
       w(7,3) = 0.5
       w(8,1) = 0.5
       w(8,3) = 0.5
    case(238)
       w(3,1) = 0.5
       w(4,1) = 0.5
       w(5,3) = 0.5
       w(6,3) = 0.5
       w(7,1) = 0.5
       w(7,3) = 0.5
       w(8,1) = 0.5
       w(8,3) = 0.5
    case(239)
       w(3,1) = 0.5
       w(4,1) = 0.5
       w(5,1) = 0.5
       w(6,1) = 0.5
    case(240)
       w(3,2) = 0.5
       w(4,2) = 0.5
       w(7,2) = 0.5
       w(8,2) = 0.5
    case(241)
       w(5,2) = 0.5
       w(6,2) = 0.5
       w(7,2) = 0.5
       w(8,2) = 0.5
    case(242)
       w(3,3) = 0.5
       w(4,3) = 0.5
       w(5,3) = 0.5
       w(6,3) = 0.5
    case(243)
       w(3,3) = 0.5
       w(4,3) = 0.5
       w(7,3) = 0.5
       w(8,3) = 0.5
    case(244)
       w(5,1) = 0.5
       w(6,1) = 0.5
       w(7,1) = 0.5
       w(8,1) = 0.5
    case(245)
       w(3,1) = 0.5
       w(4,1) = 0.5
       w(5,2) = 0.5
       w(5,3) = 0.5
       w(6,2) = 0.5
       w(6,3) = 0.5
       w(7,1) = 0.5
       w(7,2) = 0.5
       w(7,3) = 0.5
       w(8,1) = 0.5
       w(8,2) = 0.5
       w(8,3) = 0.5
    case(246)
       w(3,2) = 0.5
       w(4,2) = 0.5
       w(5,1) = 0.5
       w(5,2) = 0.5
       w(5,3) = 0.5
       w(6,1) = 0.5
       w(6,2) = 0.5
       w(6,3) = 0.5
       w(7,1) = 0.5
       w(7,3) = 0.5
       w(8,1) = 0.5
       w(8,3) = 0.5
    case(247)
       w(3,1) = 0.5
       w(3,2) = 0.5
       w(3,3) = 0.5
       w(4,1) = 0.5
       w(4,2) = 0.5
       w(4,3) = 0.5
       w(5,2) = 0.5
       w(6,2) = 0.5
       w(7,1) = 0.5
       w(7,3) = 0.5
       w(8,1) = 0.5
       w(8,3) = 0.5
    case(248)
       w(3,1) = 0.5
       w(3,2) = 0.5
       w(4,1) = 0.5
       w(4,2) = 0.5
       w(5,3) = 0.5
       w(6,3) = 0.5
       w(7,1) = 0.5
       w(7,2) = 0.5
       w(7,3) = 0.5
       w(8,1) = 0.5
       w(8,2) = 0.5
       w(8,3) = 0.5
    case(249)
       w(3,1) = 0.5
       w(3,2) = 0.5
       w(4,1) = 0.5
       w(4,2) = 0.5
       w(5,1) = 0.5
       w(5,2) = 0.5
       w(5,3) = 0.5
       w(6,1) = 0.5
       w(6,2) = 0.5
       w(6,3) = 0.5
       w(7,3) = 0.5
       w(8,3) = 0.5
    case(250)
       w(3,1) = 0.5
       w(3,2) = 0.5
       w(3,3) = 0.5
       w(4,1) = 0.5
       w(4,2) = 0.5
       w(4,3) = 0.5
       w(5,2) = 0.5
       w(5,3) = 0.5
       w(6,2) = 0.5
       w(6,3) = 0.5
       w(7,1) = 0.5
       w(8,1) = 0.5
    case(251)
       w(3,1) = 0.5
       w(3,3) = 0.5
       w(4,1) = 0.5
       w(4,3) = 0.5
       w(7,1) = 0.5
       w(7,3) = 0.5
       w(8,1) = 0.5
       w(8,3) = 0.5
    case(252)
       w(3,2) = 0.5
       w(3,3) = 0.5
       w(4,2) = 0.5
       w(4,3) = 0.5
       w(5,2) = 0.5
       w(5,3) = 0.5
       w(6,2) = 0.5
       w(6,3) = 0.5
    case(253)
       w(3,1) = 0.5
       w(3,2) = 0.5
       w(4,1) = 0.5
       w(4,2) = 0.5
       w(5,1) = 0.5
       w(5,2) = 0.5
       w(6,1) = 0.5
       w(6,2) = 0.5
    case(254)
       w(5,1) = 0.5
       w(5,3) = 0.5
       w(6,1) = 0.5
       w(6,3) = 0.5
       w(7,1) = 0.5
       w(7,3) = 0.5
       w(8,1) = 0.5
       w(8,3) = 0.5
    case(255)
       w(5,2) = 0.5
       w(5,3) = 0.5
       w(6,2) = 0.5
       w(6,3) = 0.5
       w(7,2) = 0.5
       w(7,3) = 0.5
       w(8,2) = 0.5
       w(8,3) = 0.5
    case(256)
       w(3,1) = 0.5
       w(3,2) = 0.5
       w(4,1) = 0.5
       w(4,2) = 0.5
       w(7,1) = 0.5
       w(7,2) = 0.5
       w(8,1) = 0.5
       w(8,2) = 0.5
    case(257)
       w(3,1) = 0.5
       w(4,1) = 0.5
       w(5,1) = 0.5
       w(5,3) = 0.5
       w(6,1) = 0.5
       w(6,3) = 0.5
       w(7,3) = 0.5
       w(8,3) = 0.5
    case(258)
       w(3,2) = 0.5
       w(4,2) = 0.5
       w(5,3) = 0.5
       w(6,3) = 0.5
       w(7,2) = 0.5
       w(7,3) = 0.5
       w(8,2) = 0.5
       w(8,3) = 0.5
    case(259)
       w(3,1) = 0.5
       w(4,1) = 0.5
       w(5,2) = 0.5
       w(6,2) = 0.5
       w(7,1) = 0.5
       w(7,2) = 0.5
       w(8,1) = 0.5
       w(8,2) = 0.5
    case(260)
       w(3,1) = 0.5
       w(3,3) = 0.5
       w(4,1) = 0.5
       w(4,3) = 0.5
       w(5,3) = 0.5
       w(6,3) = 0.5
       w(7,1) = 0.5
       w(8,1) = 0.5
    case(261)
       w(3,2) = 0.5
       w(3,3) = 0.5
       w(4,2) = 0.5
       w(4,3) = 0.5
       w(5,2) = 0.5
       w(6,2) = 0.5
       w(7,3) = 0.5
       w(8,3) = 0.5
    case(262)
       w(3,2) = 0.5
       w(4,2) = 0.5
       w(5,1) = 0.5
       w(5,2) = 0.5
       w(6,1) = 0.5
       w(6,2) = 0.5
       w(7,1) = 0.5
       w(8,1) = 0.5
    case(263)
       w(5,1) = 0.5
       w(5,2) = 0.5
       w(6,1) = 0.5
       w(6,2) = 0.5
       w(7,1) = 0.5
       w(7,2) = 0.5
       w(8,1) = 0.5
       w(8,2) = 0.5
    case(264)
       w(3,2) = 0.5
       w(3,3) = 0.5
       w(4,2) = 0.5
       w(4,3) = 0.5
       w(7,2) = 0.5
       w(7,3) = 0.5
       w(8,2) = 0.5
       w(8,3) = 0.5
    case(265)
       w(3,1) = 0.5
       w(3,3) = 0.5
       w(4,1) = 0.5
       w(4,3) = 0.5
       w(5,1) = 0.5
       w(5,3) = 0.5
       w(6,1) = 0.5
       w(6,3) = 0.5
    case(266)
       w(3,1) = 0.5
       w(3,2) = 0.5
       w(4,1) = 0.5
       w(4,2) = 0.5
       w(5,1) = 0.5
       w(5,3) = 0.5
       w(6,1) = 0.5
       w(6,3) = 0.5
       w(7,2) = 0.5
       w(7,3) = 0.5
       w(8,2) = 0.5
       w(8,3) = 0.5
    case(267)
       w(3,1) = 0.5
       w(3,3) = 0.5
       w(4,1) = 0.5
       w(4,3) = 0.5
       w(5,2) = 0.5
       w(5,3) = 0.5
       w(6,2) = 0.5
       w(6,3) = 0.5
       w(7,1) = 0.5
       w(7,2) = 0.5
       w(8,1) = 0.5
       w(8,2) = 0.5
    case(268)
       w(3,2) = 0.5
       w(3,3) = 0.5
       w(4,2) = 0.5
       w(4,3) = 0.5
       w(5,1) = 0.5
       w(5,2) = 0.5
       w(6,1) = 0.5
       w(6,2) = 0.5
       w(7,1) = 0.5
       w(7,3) = 0.5
       w(8,1) = 0.5
       w(8,3) = 0.5
    case(269)
       w(3,3) = 0.5
       w(4,3) = 0.5
       w(5,2) = 0.5
       w(6,2) = 0.5
       w(7,2) = 0.5
       w(7,3) = 0.5
       w(8,2) = 0.5
       w(8,3) = 0.5
    case(270)
       w(3,3) = 0.5
       w(4,3) = 0.5
       w(5,1) = 0.5
       w(5,3) = 0.5
       w(6,1) = 0.5
       w(6,3) = 0.5
       w(7,1) = 0.5
       w(8,1) = 0.5
    case(271)
       w(3,1) = 0.5
       w(3,3) = 0.5
       w(4,1) = 0.5
       w(4,3) = 0.5
       w(5,1) = 0.5
       w(6,1) = 0.5
       w(7,3) = 0.5
       w(8,3) = 0.5
    case(272)
       w(3,2) = 0.5
       w(4,2) = 0.5
       w(5,1) = 0.5
       w(6,1) = 0.5
       w(7,1) = 0.5
       w(7,2) = 0.5
       w(8,1) = 0.5
       w(8,2) = 0.5
    case(273)
       w(3,1) = 0.5
       w(4,1) = 0.5
       w(5,1) = 0.5
       w(5,2) = 0.5
       w(6,1) = 0.5
       w(6,2) = 0.5
       w(7,2) = 0.5
       w(8,2) = 0.5
    case(274)
       w(3,2) = 0.5
       w(3,3) = 0.5
       w(4,2) = 0.5
       w(4,3) = 0.5
       w(5,3) = 0.5
       w(6,3) = 0.5
       w(7,2) = 0.5
       w(8,2) = 0.5
    case(275)
       w(5,1) = 0.5
       w(5,2) = 0.5
       w(5,3) = 0.5
       w(6,1) = 0.5
       w(6,2) = 0.5
       w(6,3) = 0.5
       w(7,1) = 0.5
       w(7,2) = 0.5
       w(7,3) = 0.5
       w(8,1) = 0.5
       w(8,2) = 0.5
       w(8,3) = 0.5
    case(276)
       w(3,1) = 0.5
       w(3,2) = 0.5
       w(3,3) = 0.5
       w(4,1) = 0.5
       w(4,2) = 0.5
       w(4,3) = 0.5
       w(7,1) = 0.5
       w(7,2) = 0.5
       w(7,3) = 0.5
       w(8,1) = 0.5
       w(8,2) = 0.5
       w(8,3) = 0.5
    case(277)
       w(3,1) = 0.5
       w(3,2) = 0.5
       w(3,3) = 0.5
       w(4,1) = 0.5
       w(4,2) = 0.5
       w(4,3) = 0.5
       w(5,1) = 0.5
       w(5,2) = 0.5
       w(5,3) = 0.5
       w(6,1) = 0.5
       w(6,2) = 0.5
       w(6,3) = 0.5
    case(278)
       w(3,1) = 0.5
       w(3,2) = 0.5
       w(4,1) = 0.5
       w(4,2) = 0.5
       w(5,1) = 0.5
       w(5,2) = 0.5
       w(6,1) = 0.5
       w(6,2) = 0.5
    case(279)
       w(3,1) = 0.5
       w(3,2) = 0.5
       w(4,1) = 0.5
       w(4,2) = 0.5
       w(5,1) = 0.5
       w(6,1) = 0.5
       w(7,2) = 0.5
       w(8,2) = 0.5
    case(280)
       w(2,2) = 0.5
       w(2,3) = 0.5
       w(4,2) = 0.5
       w(4,3) = 0.5
       w(5,2) = 0.5
       w(5,3) = 0.5
       w(7,2) = 0.5
       w(7,3) = 0.5
    case(281)
       w(3,3) = 0.5
       w(4,3) = 0.5
       w(5,2) = 0.5
       w(5,3) = 0.5
       w(6,2) = 0.5
       w(6,3) = 0.5
       w(7,2) = 0.5
       w(8,2) = 0.5
    case(282)
       w(2,1) = 0.5
       w(2,3) = 0.5
       w(3,1) = 0.5
       w(3,3) = 0.5
       w(5,1) = 0.5
       w(5,3) = 0.5
       w(8,1) = 0.5
       w(8,3) = 0.5
    case(283)
       w(3,3) = 0.5
       w(4,3) = 0.5
       w(5,1) = 0.5
       w(6,1) = 0.5
       w(7,1) = 0.5
       w(7,3) = 0.5
       w(8,1) = 0.5
       w(8,3) = 0.5
    case(284)
       w(3,1) = 0.5
       w(3,2) = 0.5
       w(3,3) = 0.5
       w(4,1) = 0.5
       w(4,2) = 0.5
       w(4,3) = 0.5
       w(5,1) = 0.5
       w(5,2) = 0.5
       w(6,1) = 0.5
       w(6,2) = 0.5
       w(7,3) = 0.5
       w(8,3) = 0.5
    case(285)
       w(3,1) = 0.5
       w(3,2) = 0.5
       w(3,3) = 0.5
       w(4,1) = 0.5
       w(4,2) = 0.5
       w(4,3) = 0.5
       w(5,3) = 0.5
       w(6,3) = 0.5
       w(7,1) = 0.5
       w(7,2) = 0.5
       w(8,1) = 0.5
       w(8,2) = 0.5
    case(286)
       w(3,1) = 0.5
       w(4,1) = 0.5
       w(5,1) = 0.5
       w(5,2) = 0.5
       w(5,3) = 0.5
       w(6,1) = 0.5
       w(6,2) = 0.5
       w(6,3) = 0.5
       w(7,2) = 0.5
       w(7,3) = 0.5
       w(8,2) = 0.5
       w(8,3) = 0.5
    case(287)
       w(3,2) = 0.5
       w(3,3) = 0.5
       w(4,2) = 0.5
       w(4,3) = 0.5
       w(5,1) = 0.5
       w(5,2) = 0.5
       w(5,3) = 0.5
       w(6,1) = 0.5
       w(6,2) = 0.5
       w(6,3) = 0.5
       w(7,1) = 0.5
       w(8,1) = 0.5
    case(288)
       w(3,1) = 0.5
       w(3,3) = 0.5
       w(4,1) = 0.5
       w(4,3) = 0.5
       w(5,2) = 0.5
       w(6,2) = 0.5
       w(7,1) = 0.5
       w(7,2) = 0.5
       w(7,3) = 0.5
       w(8,1) = 0.5
       w(8,2) = 0.5
       w(8,3) = 0.5
    case(289)
       w(3,2) = 0.5
       w(4,2) = 0.5
       w(5,1) = 0.5
       w(5,3) = 0.5
       w(6,1) = 0.5
       w(6,3) = 0.5
       w(7,1) = 0.5
       w(7,2) = 0.5
       w(7,3) = 0.5
       w(8,1) = 0.5
       w(8,2) = 0.5
       w(8,3) = 0.5
    case(290)
       w(3,1) = 0.5
       w(3,3) = 0.5
       w(4,1) = 0.5
       w(4,3) = 0.5
       w(5,1) = 0.5
       w(5,2) = 0.5
       w(6,1) = 0.5
       w(6,2) = 0.5
       w(7,2) = 0.5
       w(7,3) = 0.5
       w(8,2) = 0.5
       w(8,3) = 0.5
    case(291)
       w(3,2) = 0.5
       w(3,3) = 0.5
       w(4,2) = 0.5
       w(4,3) = 0.5
       w(5,1) = 0.5
       w(5,3) = 0.5
       w(6,1) = 0.5
       w(6,3) = 0.5
       w(7,1) = 0.5
       w(7,2) = 0.5
       w(8,1) = 0.5
       w(8,2) = 0.5
    case(292)
       w(3,1) = 0.5
       w(3,3) = 0.5
       w(4,1) = 0.5
       w(4,3) = 0.5
       w(5,1) = 0.5
       w(5,2) = 0.5
       w(5,3) = 0.5
       w(6,1) = 0.5
       w(6,2) = 0.5
       w(6,3) = 0.5
       w(7,2) = 0.5
       w(8,2) = 0.5
    case(293)
       w(3,2) = 0.5
       w(3,3) = 0.5
       w(4,2) = 0.5
       w(4,3) = 0.5
       w(5,1) = 0.5
       w(6,1) = 0.5
       w(7,1) = 0.5
       w(7,2) = 0.5
       w(7,3) = 0.5
       w(8,1) = 0.5
       w(8,2) = 0.5
       w(8,3) = 0.5
    case(294)
       w(3,3) = 0.5
       w(4,3) = 0.5
       w(5,1) = 0.5
       w(5,2) = 0.5
       w(6,1) = 0.5
       w(6,2) = 0.5
       w(7,1) = 0.5
       w(7,2) = 0.5
       w(7,3) = 0.5
       w(8,1) = 0.5
       w(8,2) = 0.5
       w(8,3) = 0.5
    case(295)
       w(3,1) = 0.5
       w(3,2) = 0.5
       w(3,3) = 0.5
       w(4,1) = 0.5
       w(4,2) = 0.5
       w(4,3) = 0.5
       w(5,1) = 0.5
       w(5,3) = 0.5
       w(6,1) = 0.5
       w(6,3) = 0.5
       w(7,2) = 0.5
       w(8,2) = 0.5
    case(296)
       w(3,1) = 0.5
       w(3,2) = 0.5
       w(3,3) = 0.5
       w(4,1) = 0.5
       w(4,2) = 0.5
       w(4,3) = 0.5
       w(5,1) = 0.5
       w(6,1) = 0.5
       w(7,2) = 0.5
       w(7,3) = 0.5
       w(8,2) = 0.5
       w(8,3) = 0.5
    case(297)
       w(3,3) = 0.5
       w(4,3) = 0.5
       w(5,1) = 0.5
       w(5,2) = 0.5
       w(5,3) = 0.5
       w(6,1) = 0.5
       w(6,2) = 0.5
       w(6,3) = 0.5
       w(7,1) = 0.5
       w(7,2) = 0.5
       w(8,1) = 0.5
       w(8,2) = 0.5
    case(298)
       w(3,3) = 0.5
       w(4,3) = 0.5
       w(7,3) = 0.5
       w(8,3) = 0.5
    case(299)
       w(3,3) = 0.5
       w(4,3) = 0.5
       w(5,3) = 0.5
       w(6,3) = 0.5
    case(300)
       w(3,1) = 0.5
       w(4,1) = 0.5
       w(5,1) = 0.5
       w(6,1) = 0.5
    case(301)
       w(5,1) = 0.5
       w(6,1) = 0.5
       w(7,1) = 0.5
       w(8,1) = 0.5
    case(302)
       w(5,2) = 0.5
       w(6,2) = 0.5
       w(7,2) = 0.5
       w(8,2) = 0.5
    case(303)
       w(3,2) = 0.5
       w(4,2) = 0.5
       w(7,2) = 0.5
       w(8,2) = 0.5
    case(304)
       w(3,2) = 0.5
       w(3,3) = 0.5
       w(4,2) = 0.5
       w(4,3) = 0.5
       w(7,2) = 0.5
       w(7,3) = 0.5
       w(8,2) = 0.5
       w(8,3) = 0.5
    case(305)
       w(3,2) = 0.5
       w(3,3) = 0.5
       w(4,2) = 0.5
       w(4,3) = 0.5
       w(5,2) = 0.5
       w(5,3) = 0.5
       w(6,2) = 0.5
       w(6,3) = 0.5
    case(306)
       w(3,1) = 0.5
       w(3,3) = 0.5
       w(4,1) = 0.5
       w(4,3) = 0.5
       w(5,1) = 0.5
       w(5,3) = 0.5
       w(6,1) = 0.5
       w(6,3) = 0.5
    case(307)
       w(5,1) = 0.5
       w(5,3) = 0.5
       w(6,1) = 0.5
       w(6,3) = 0.5
       w(7,1) = 0.5
       w(7,3) = 0.5
       w(8,1) = 0.5
       w(8,3) = 0.5
    case(308)
       w(5,2) = 0.5
       w(5,3) = 0.5
       w(6,2) = 0.5
       w(6,3) = 0.5
       w(7,2) = 0.5
       w(7,3) = 0.5
       w(8,2) = 0.5
       w(8,3) = 0.5
    case(309)
       w(3,2) = 0.5
       w(3,3) = 0.5
       w(4,2) = 0.5
       w(4,3) = 0.5
       w(7,2) = 0.5
       w(7,3) = 0.5
       w(8,2) = 0.5
       w(8,3) = 0.5
    case(313)
       w(5,3) = 0.5
       w(6,3) = 0.5
       w(7,3) = 0.5
       w(8,3) = 0.5
    case(314)
       w(3,1) = 0.5
       w(4,1) = 0.5
       w(7,1) = 0.5
       w(8,1) = 0.5
    case(315)
       w(3,2) = 0.5
       w(4,2) = 0.5
       w(5,2) = 0.5
       w(6,2) = 0.5
    case(316)
       w(3,2) = 0.5
       w(4,2) = 0.5
       w(7,2) = 0.5
       w(8,2) = 0.5
    case(317)
       w(3,2) = 0.5
       w(4,2) = 0.5
       w(5,2) = 0.5
       w(6,2) = 0.5
    case(318)
       w(3,3) = 0.5
       w(4,3) = 0.5
       w(5,3) = 0.5
       w(6,3) = 0.5
    case(319)
       w(5,3) = 0.5
       w(6,3) = 0.5
       w(7,3) = 0.5
       w(8,3) = 0.5
    case(320)
       w(5,3) = 0.5
       w(6,3) = 0.5
       w(7,3) = 0.5
       w(8,3) = 0.5
    case(321)
       w(3,3) = 0.5
       w(4,3) = 0.5
       w(7,3) = 0.5
       w(8,3) = 0.5
    case(322)
       w(5,2) = 0.5
       w(5,3) = 0.5
       w(6,2) = 0.5
       w(6,3) = 0.5
       w(7,2) = 0.5
       w(7,3) = 0.5
       w(8,2) = 0.5
       w(8,3) = 0.5
    case(323)
       w(3,2) = 0.5
       w(4,2) = 0.5
       w(5,2) = 0.5
       w(5,3) = 0.5
       w(6,2) = 0.5
       w(6,3) = 0.5
       w(7,3) = 0.5
       w(8,3) = 0.5
    case(324)
       w(5,2) = 0.5
       w(5,3) = 0.5
       w(6,2) = 0.5
       w(6,3) = 0.5
       w(7,2) = 0.5
       w(7,3) = 0.5
       w(8,2) = 0.5
       w(8,3) = 0.5
    case(325)
       w(3,2) = 0.5
       w(4,2) = 0.5
       w(5,3) = 0.5
       w(6,3) = 0.5
       w(7,2) = 0.5
       w(7,3) = 0.5
       w(8,2) = 0.5
       w(8,3) = 0.5
    case(326)
       w(5,1) = 0.5
       w(5,3) = 0.5
       w(6,1) = 0.5
       w(6,3) = 0.5
       w(7,1) = 0.5
       w(7,3) = 0.5
       w(8,1) = 0.5
       w(8,3) = 0.5
    case(327)
       w(3,1) = 0.5
       w(4,1) = 0.5
       w(5,3) = 0.5
       w(6,3) = 0.5
       w(7,1) = 0.5
       w(7,3) = 0.5
       w(8,1) = 0.5
       w(8,3) = 0.5
    case(328)
       w(5,1) = 0.5
       w(5,3) = 0.5
       w(6,1) = 0.5
       w(6,3) = 0.5
       w(7,1) = 0.5
       w(7,3) = 0.5
       w(8,1) = 0.5
       w(8,3) = 0.5
    case(329)
       w(3,1) = 0.5
       w(3,3) = 0.5
       w(4,1) = 0.5
       w(4,3) = 0.5
       w(5,3) = 0.5
       w(6,3) = 0.5
       w(7,1) = 0.5
       w(8,1) = 0.5
    case(330)
       w(5,2) = 0.5
       w(5,3) = 0.5
       w(6,2) = 0.5
       w(6,3) = 0.5
       w(7,2) = 0.5
       w(7,3) = 0.5
       w(8,2) = 0.5
       w(8,3) = 0.5
    case(331)
       w(3,2) = 0.5
       w(3,3) = 0.5
       w(4,2) = 0.5
       w(4,3) = 0.5
       w(5,2) = 0.5
       w(6,2) = 0.5
       w(7,3) = 0.5
       w(8,3) = 0.5
    case(332)
       w(5,2) = 0.5
       w(5,3) = 0.5
       w(6,2) = 0.5
       w(6,3) = 0.5
       w(7,2) = 0.5
       w(7,3) = 0.5
       w(8,2) = 0.5
       w(8,3) = 0.5
    case(333)
       w(3,2) = 0.5
       w(4,2) = 0.5
       w(5,2) = 0.5
       w(5,3) = 0.5
       w(6,2) = 0.5
       w(6,3) = 0.5
       w(7,3) = 0.5
       w(8,3) = 0.5
    case(335)
       w(5,1) = 0.25
       w(5,2) = 0.25
       w(5,3) = 0.25
       w(6,1) = 0.25
       w(6,2) = 0.25
       w(6,3) = 0.25
       w(7,1) = 0.25
       w(7,2) = 0.25
       w(7,3) = 0.25
       w(8,1) = 0.25
       w(8,2) = 0.25
       w(8,3) = 0.25
    case(336)
       w(3,1) = 0.25
       w(3,2) = 0.25
       w(4,1) = 0.25
       w(4,2) = 0.25
       w(5,2) = 0.25
       w(5,3) = 0.25
       w(6,2) = 0.25
       w(6,3) = 0.25
       w(7,1) = 0.25
       w(7,3) = 0.25
       w(8,1) = 0.25
       w(8,3) = 0.25
    case(338)
       w(5,3) = 0.5
       w(6,3) = 0.5
       w(7,3) = 0.5
       w(8,3) = 0.5
    case(339)
       w(3,1) = 0.5
       w(4,1) = 0.5
       w(7,1) = 0.5
       w(8,1) = 0.5
    case(340)
       w(3,2) = 0.5
       w(4,2) = 0.5
       w(5,2) = 0.5
       w(6,2) = 0.5
    case(341)
       w(3,2) = 0.5
       w(4,2) = 0.5
       w(5,3) = 0.5
       w(6,3) = 0.5
       w(7,2) = 0.5
       w(7,3) = 0.5
       w(8,2) = 0.5
       w(8,3) = 0.5
    case(342)
       w(3,1) = 0.5
       w(4,1) = 0.5
       w(5,2) = 0.5
       w(6,2) = 0.5
       w(7,1) = 0.5
       w(7,2) = 0.5
       w(8,1) = 0.5
       w(8,2) = 0.5
    case(343)
       w(3,2) = 0.5
       w(4,2) = 0.5
       w(7,2) = 0.5
       w(8,2) = 0.5
    case(344)
       w(3,1) = 0.5
       w(4,1) = 0.5
       w(5,1) = 0.5
       w(6,1) = 0.5
    case(345)
       w(3,3) = 0.5
       w(4,3) = 0.5
       w(5,3) = 0.5
       w(6,3) = 0.5
    case(346)
       w(5,2) = 0.5
       w(6,2) = 0.5
       w(7,2) = 0.5
       w(8,2) = 0.5
    case(347)
       w(5,1) = 0.5
       w(6,1) = 0.5
       w(7,1) = 0.5
       w(8,1) = 0.5
    case(348)
       w(3,3) = 0.5
       w(4,3) = 0.5
       w(7,3) = 0.5
       w(8,3) = 0.5
    case(350)
       w(2,3) = 0.25
       w(3,3) = 0.5
       w(4,3) = 0.75
    case(351)
       w(2,3) = 0.5
       w(4,3) = 0.5
    case(352)
       w(2,3) = 0.75
       w(3,3) = 0.5
       w(4,3) = 0.25
    case(354)
       w(2,2) = 0.5
       w(2,3) = 0.25
       w(3,1) = 0.5
       w(3,2) = 0.5
       w(3,3) = 0.5
       w(4,1) = 0.5
       w(4,3) = 0.75
    case(358)
       w(3,3) = 0.5
       w(4,3) = 0.5
       w(7,3) = 0.5
       w(8,3) = 0.5
    case(359)
       w(2,1) = 0.5
       w(2,2) = 0.5
       w(4,1) = 0.5
       w(4,2) = 0.5
       w(5,1) = 0.5
       w(5,2) = 0.5
       w(7,1) = 0.5
       w(7,2) = 0.5
    case(360)
       w(3,1) = 0.5
       w(4,1) = 0.5
       w(5,1) = 0.5
       w(5,2) = 0.5
       w(6,1) = 0.5
       w(6,2) = 0.5
       w(7,2) = 0.5
       w(8,2) = 0.5
    case(361)
       w(2,1) = 0.5
       w(2,2) = 0.5
       w(2,3) = 0.5
       w(4,1) = 0.5
       w(4,2) = 0.5
       w(4,3) = 0.5
       w(5,1) = 0.5
       w(5,2) = 0.5
       w(5,3) = 0.5
       w(7,1) = 0.5
       w(7,2) = 0.5
       w(7,3) = 0.5
    case(362)
       w(3,2) = 0.5
       w(3,3) = 0.5
       w(4,2) = 0.5
       w(4,3) = 0.5
       w(5,1) = 0.5
       w(5,2) = 0.5
       w(6,1) = 0.5
       w(6,2) = 0.5
       w(7,1) = 0.5
       w(7,3) = 0.5
       w(8,1) = 0.5
       w(8,3) = 0.5
    case(364)
       w(2,2) = 0.5
       w(2,3) = 0.25
       w(3,1) = 0.5
       w(3,2) = 0.5
       w(3,3) = 0.5
       w(4,1) = 0.5
       w(4,3) = 0.75
       w(5,2) = 0.5
       w(5,3) = 0.25
       w(7,1) = 0.5
       w(7,3) = 0.75
       w(8,1) = 0.5
       w(8,2) = 0.5
       w(8,3) = 0.5
    case(365)
       w(3,1) = 0.75
       w(3,2) = 0.25
       w(3,3) = 0.25
       w(4,1) = 0.75
       w(4,2) = 0.25
       w(4,3) = 0.25
       w(5,1) = 0.5
       w(5,3) = 0.5
       w(6,1) = 0.5
       w(6,3) = 0.5
       w(7,1) = 0.75
       w(7,2) = 0.75
       w(7,3) = 0.75
       w(8,1) = 0.75
       w(8,2) = 0.75
       w(8,3) = 0.75
    case(367)
       w(2,1) = 0.5
       w(2,2) = 0.5
       w(4,1) = 0.5
       w(4,2) = 0.5
       w(5,1) = 0.5
       w(5,2) = 0.5
       w(7,1) = 0.5
       w(7,2) = 0.5
    case(368)
       w(2,3) = 0.25
       w(3,3) = 0.5
       w(4,3) = 0.75
       w(5,3) = 0.5
       w(6,3) = 0.25
       w(8,3) = 0.75
    case(369)
       w(2,1) = 0.5
       w(2,2) = 0.5
       w(2,3) = 0.25
       w(3,3) = 0.5
       w(4,1) = 0.5
       w(4,2) = 0.5
       w(4,3) = 0.75
       w(5,1) = 0.5
       w(5,2) = 0.5
       w(5,3) = 0.75
       w(6,3) = 0.5
       w(7,1) = 0.5
       w(7,2) = 0.5
       w(7,3) = 0.25
    case(370)
       w(2,3) = 0.5
       w(4,3) = 0.5
       w(6,3) = 0.5
       w(8,3) = 0.5
    case(371)
       w(2,1) = 0.5
       w(2,2) = 0.5
       w(2,3) = 0.5
       w(4,1) = 0.5
       w(4,2) = 0.5
       w(4,3) = 0.5
       w(5,1) = 0.5
       w(5,2) = 0.5
       w(5,3) = 0.5
       w(7,1) = 0.5
       w(7,2) = 0.5
       w(7,3) = 0.5
    case(372)
       w(2,3) = 0.75
       w(3,3) = 0.5
       w(4,3) = 0.25
       w(5,3) = 0.5
       w(6,3) = 0.75
       w(8,3) = 0.25
    case(373)
       w(2,1) = 0.5
       w(2,2) = 0.5
       w(2,3) = 0.75
       w(3,3) = 0.5
       w(4,1) = 0.5
       w(4,2) = 0.5
       w(4,3) = 0.25
       w(5,1) = 0.5
       w(5,2) = 0.5
       w(5,3) = 0.25
       w(6,3) = 0.5
       w(7,1) = 0.5
       w(7,2) = 0.5
       w(7,3) = 0.75
    case(375)
       w(2,2) = 0.5
       w(2,3) = 0.25
       w(3,1) = 0.5
       w(3,2) = 0.5
       w(3,3) = 0.5
       w(4,1) = 0.5
       w(4,3) = 0.75
       w(5,2) = 0.5
       w(5,3) = 0.25
       w(7,1) = 0.5
       w(7,3) = 0.75
       w(8,1) = 0.5
       w(8,2) = 0.5
       w(8,3) = 0.5
    case(377)
       w(5,1) = 0.5
       w(5,2) = 0.5
       w(6,1) = 0.5
       w(6,2) = 0.5
       w(7,1) = 0.5
       w(7,2) = 0.5
       w(8,1) = 0.5
       w(8,2) = 0.5
    case(378)
       w(2,3) = 0.5
       w(4,3) = 0.5
       w(5,3) = 0.5
       w(7,3) = 0.5
    case(379)
       w(2,1) = 0.5
       w(2,2) = 0.5
       w(2,3) = 0.5
       w(4,1) = 0.5
       w(4,2) = 0.5
       w(4,3) = 0.5
       w(5,1) = 0.5
       w(5,2) = 0.5
       w(5,3) = 0.5
       w(7,1) = 0.5
       w(7,2) = 0.5
       w(7,3) = 0.5
    case(380)
       w(5,3) = 0.5
       w(6,3) = 0.5
       w(7,3) = 0.5
       w(8,3) = 0.5
    case(381)
       w(5,1) = 0.5
       w(5,2) = 0.5
       w(5,3) = 0.5
       w(6,1) = 0.5
       w(6,2) = 0.5
       w(6,3) = 0.5
       w(7,1) = 0.5
       w(7,2) = 0.5
       w(7,3) = 0.5
       w(8,1) = 0.5
       w(8,2) = 0.5
       w(8,3) = 0.5
    case(382)
       w(2,3) = 0.5
       w(4,3) = 0.5
       w(6,3) = 0.5
       w(8,3) = 0.5
    case(383)
       w(2,3) = 0.5
       w(4,3) = 0.5
       w(5,1) = 0.5
       w(5,2) = 0.5
       w(6,1) = 0.5
       w(6,2) = 0.5
       w(6,3) = 0.5
       w(7,1) = 0.5
       w(7,2) = 0.5
       w(8,1) = 0.5
       w(8,2) = 0.5
       w(8,3) = 0.5
    case(385)
       w(5,3) = 0.5
       w(6,3) = 0.5
       w(7,3) = 0.5
       w(8,3) = 0.5
    case(386)
       w(2,2) = 0.5
       w(2,3) = 0.25
       w(3,1) = 0.5
       w(3,2) = 0.5
       w(3,3) = 0.5
       w(4,1) = 0.5
       w(4,3) = 0.75
       w(6,2) = 0.5
       w(6,3) = 0.25
       w(7,1) = 0.5
       w(7,2) = 0.5
       w(7,3) = 0.5
       w(8,1) = 0.5
       w(8,3) = 0.75
    case(387)
       w(2,2) = 0.5
       w(2,3) = 0.25
       w(3,1) = 0.5
       w(3,2) = 0.5
       w(3,3) = 0.5
       w(4,1) = 0.5
       w(4,3) = 0.75
       w(5,3) = 0.5
       w(6,2) = 0.5
       w(6,3) = 0.75
       w(7,1) = 0.5
       w(7,2) = 0.5
       w(8,1) = 0.5
       w(8,3) = 0.25
    case(389)
       w(5,3) = 0.5
       w(6,3) = 0.5
       w(7,3) = 0.5
       w(8,3) = 0.5
    case(390)
       w(5,1) = 0.5
       w(5,2) = 0.5
       w(6,1) = 0.5
       w(6,2) = 0.5
       w(7,1) = 0.5
       w(7,2) = 0.5
       w(8,1) = 0.5
       w(8,2) = 0.5
    case(391)
       w(5,1) = 0.5
       w(5,2) = 0.5
       w(5,3) = 0.5
       w(6,1) = 0.5
       w(6,2) = 0.5
       w(6,3) = 0.5
       w(7,1) = 0.5
       w(7,2) = 0.5
       w(7,3) = 0.5
       w(8,1) = 0.5
       w(8,2) = 0.5
       w(8,3) = 0.5
    case(393)
       w(5,3) = 0.5
       w(6,3) = 0.5
       w(7,3) = 0.5
       w(8,3) = 0.5
    case(394)
       w(5,1) = 0.5
       w(5,2) = 0.5
       w(6,1) = 0.5
       w(6,2) = 0.5
       w(7,1) = 0.5
       w(7,2) = 0.5
       w(8,1) = 0.5
       w(8,2) = 0.5
    case(395)
       w(5,1) = 0.5
       w(5,2) = 0.5
       w(5,3) = 0.5
       w(6,1) = 0.5
       w(6,2) = 0.5
       w(6,3) = 0.5
       w(7,1) = 0.5
       w(7,2) = 0.5
       w(7,3) = 0.5
       w(8,1) = 0.5
       w(8,2) = 0.5
       w(8,3) = 0.5
    case(397)
       w(5,3) = 0.5
       w(6,3) = 0.5
       w(7,3) = 0.5
       w(8,3) = 0.5
    case(399)
       w(5,2) = 0.5
       w(5,3) = 0.25
       w(6,2) = 0.5
       w(6,3) = 0.25
       w(7,2) = 0.5
       w(7,3) = 0.25
       w(8,2) = 0.5
       w(8,3) = 0.25
    case(401)
       w(9,3) = 0.5
       w(10,3) = 0.5
       w(11,3) = 0.5
       w(12,3) = 0.5
       w(13,3) = 0.5
       w(14,3) = 0.5
       w(15,3) = 0.5
       w(16,3) = 0.5
    case(402)
       w(9,1) = 0.5
       w(9,2) = 0.5
       w(10,1) = 0.5
       w(10,2) = 0.5
       w(11,1) = 0.5
       w(11,2) = 0.5
       w(12,1) = 0.5
       w(12,2) = 0.5
       w(13,1) = 0.5
       w(13,2) = 0.5
       w(14,1) = 0.5
       w(14,2) = 0.5
       w(15,1) = 0.5
       w(15,2) = 0.5
       w(16,1) = 0.5
       w(16,2) = 0.5
    case(403)
       w(3,1) = 0.5
       w(4,1) = 0.5
       w(5,1) = 0.5
       w(5,2) = 0.5
       w(6,1) = 0.5
       w(6,2) = 0.5
       w(7,2) = 0.5
       w(8,2) = 0.5
       w(9,2) = 0.5
       w(10,2) = 0.5
       w(11,1) = 0.5
       w(11,2) = 0.5
       w(12,1) = 0.5
       w(12,2) = 0.5
       w(13,1) = 0.5
       w(14,1) = 0.5
    case(404)
       w(9,1) = 0.5
       w(9,2) = 0.5
       w(9,3) = 0.5
       w(10,1) = 0.5
       w(10,2) = 0.5
       w(10,3) = 0.5
       w(11,1) = 0.5
       w(11,2) = 0.5
       w(11,3) = 0.5
       w(12,1) = 0.5
       w(12,2) = 0.5
       w(12,3) = 0.5
       w(13,1) = 0.5
       w(13,2) = 0.5
       w(13,3) = 0.5
       w(14,1) = 0.5
       w(14,2) = 0.5
       w(14,3) = 0.5
       w(15,1) = 0.5
       w(15,2) = 0.5
       w(15,3) = 0.5
       w(16,1) = 0.5
       w(16,2) = 0.5
       w(16,3) = 0.5
    case(405)
       w(3,1) = 0.5
       w(4,1) = 0.5
       w(5,1) = 0.5
       w(5,2) = 0.5
       w(6,1) = 0.5
       w(6,2) = 0.5
       w(7,2) = 0.5
       w(8,2) = 0.5
       w(9,2) = 0.5
       w(9,3) = 0.5
       w(10,2) = 0.5
       w(10,3) = 0.5
       w(11,1) = 0.5
       w(11,2) = 0.5
       w(11,3) = 0.5
       w(12,1) = 0.5
       w(12,2) = 0.5
       w(12,3) = 0.5
       w(13,1) = 0.5
       w(13,3) = 0.5
       w(14,1) = 0.5
       w(14,3) = 0.5
       w(15,3) = 0.5
       w(16,3) = 0.5
    case(406)
       w(9,1) = 0.5
       w(9,2) = 0.5
       w(10,1) = 0.5
       w(10,2) = 0.5
       w(11,1) = 0.5
       w(11,2) = 0.5
       w(12,1) = 0.5
       w(12,2) = 0.5
       w(13,1) = 0.5
       w(13,2) = 0.5
       w(14,1) = 0.5
       w(14,2) = 0.5
       w(15,1) = 0.5
       w(15,2) = 0.5
       w(16,1) = 0.5
       w(16,2) = 0.5
    case(407)
       w(9,1) = 0.5
       w(9,2) = 0.5
       w(9,3) = 0.5
       w(10,1) = 0.5
       w(10,2) = 0.5
       w(10,3) = 0.5
       w(11,1) = 0.5
       w(11,2) = 0.5
       w(11,3) = 0.5
       w(12,1) = 0.5
       w(12,2) = 0.5
       w(12,3) = 0.5
       w(13,1) = 0.5
       w(13,2) = 0.5
       w(13,3) = 0.5
       w(14,1) = 0.5
       w(14,2) = 0.5
       w(14,3) = 0.5
       w(15,1) = 0.5
       w(15,2) = 0.5
       w(15,3) = 0.5
       w(16,1) = 0.5
       w(16,2) = 0.5
       w(16,3) = 0.5
    case(408)
       w(2,1) = 0.5
       w(2,2) = 0.5
       w(4,1) = 0.5
       w(4,2) = 0.5
       w(5,1) = 0.5
       w(5,2) = 0.5
       w(7,1) = 0.5
       w(7,2) = 0.5
       w(9,1) = 0.5
       w(9,2) = 0.5
       w(11,1) = 0.5
       w(11,2) = 0.5
       w(14,1) = 0.5
       w(14,2) = 0.5
       w(16,1) = 0.5
       w(16,2) = 0.5
    case(409)
       w(3,1) = 0.5
       w(4,1) = 0.5
       w(5,1) = 0.5
       w(5,2) = 0.5
       w(6,1) = 0.5
       w(6,2) = 0.5
       w(7,2) = 0.5
       w(8,2) = 0.5
       w(9,1) = 0.5
       w(10,1) = 0.5
       w(13,2) = 0.5
       w(14,2) = 0.5
       w(15,1) = 0.5
       w(15,2) = 0.5
       w(16,1) = 0.5
       w(16,2) = 0.5
    case(410)
       w(2,1) = 0.5
       w(2,2) = 0.5
       w(4,1) = 0.5
       w(4,2) = 0.5
       w(5,1) = 0.5
       w(5,2) = 0.5
       w(5,3) = 0.5
       w(6,3) = 0.5
       w(7,1) = 0.5
       w(7,2) = 0.5
       w(7,3) = 0.5
       w(8,3) = 0.5
       w(9,1) = 0.5
       w(9,2) = 0.5
       w(11,1) = 0.5
       w(11,2) = 0.5
       w(13,3) = 0.5
       w(14,1) = 0.5
       w(14,2) = 0.5
       w(14,3) = 0.5
       w(15,3) = 0.5
       w(16,1) = 0.5
       w(16,2) = 0.5
       w(16,3) = 0.5
    case(411)
       w(3,1) = 0.5
       w(4,1) = 0.5
       w(5,1) = 0.5
       w(5,2) = 0.5
       w(6,1) = 0.5
       w(6,2) = 0.5
       w(7,2) = 0.5
       w(8,2) = 0.5
       w(9,1) = 0.5
       w(9,3) = 0.5
       w(10,1) = 0.5
       w(10,3) = 0.5
       w(11,3) = 0.5
       w(12,3) = 0.5
       w(13,2) = 0.5
       w(13,3) = 0.5
       w(14,2) = 0.5
       w(14,3) = 0.5
       w(15,1) = 0.5
       w(15,2) = 0.5
       w(15,3) = 0.5
       w(16,1) = 0.5
       w(16,2) = 0.5
       w(16,3) = 0.5
    case(412)
       w(3,3) = 0.5
       w(4,3) = 0.5
       w(7,3) = 0.5
       w(8,3) = 0.5
       w(11,3) = 0.5
       w(12,3) = 0.5
       w(15,3) = 0.5
       w(16,3) = 0.5
    case(413)
       w(3,3) = 0.5
       w(4,3) = 0.5
       w(7,3) = 0.5
       w(8,3) = 0.5
       w(9,3) = 0.5
       w(10,3) = 0.5
       w(13,3) = 0.5
       w(14,3) = 0.5
    case(414)
       w(2,1) = 0.5
       w(2,2) = 0.5
       w(2,3) = 0.5
       w(4,1) = 0.5
       w(4,2) = 0.5
       w(4,3) = 0.5
       w(5,3) = 0.5
       w(6,1) = 0.5
       w(6,2) = 0.5
       w(7,3) = 0.5
       w(8,1) = 0.5
       w(8,2) = 0.5
       w(9,1) = 0.5
       w(9,2) = 0.5
       w(9,3) = 0.5
       w(11,1) = 0.5
       w(11,2) = 0.5
       w(11,3) = 0.5
       w(13,1) = 0.5
       w(13,2) = 0.5
       w(14,3) = 0.5
       w(15,1) = 0.5
       w(15,2) = 0.5
       w(16,3) = 0.5
    case(415)
       w(3,1) = 0.5
       w(3,3) = 0.5
       w(4,1) = 0.5
       w(4,3) = 0.5
       w(5,1) = 0.5
       w(5,2) = 0.5
       w(6,1) = 0.5
       w(6,2) = 0.5
       w(7,2) = 0.5
       w(7,3) = 0.5
       w(8,2) = 0.5
       w(8,3) = 0.5
       w(9,2) = 0.5
       w(10,2) = 0.5
       w(11,1) = 0.5
       w(11,2) = 0.5
       w(11,3) = 0.5
       w(12,1) = 0.5
       w(12,2) = 0.5
       w(12,3) = 0.5
       w(13,1) = 0.5
       w(14,1) = 0.5
       w(15,3) = 0.5
       w(16,3) = 0.5
    case(416)
       w(2,1) = 0.5
       w(2,2) = 0.5
       w(2,3) = 0.5
       w(4,1) = 0.5
       w(4,2) = 0.5
       w(4,3) = 0.5
       w(6,1) = 0.5
       w(6,2) = 0.5
       w(6,3) = 0.5
       w(8,1) = 0.5
       w(8,2) = 0.5
       w(8,3) = 0.5
       w(9,1) = 0.5
       w(9,2) = 0.5
       w(9,3) = 0.5
       w(11,1) = 0.5
       w(11,2) = 0.5
       w(11,3) = 0.5
       w(13,1) = 0.5
       w(13,2) = 0.5
       w(13,3) = 0.5
       w(15,1) = 0.5
       w(15,2) = 0.5
       w(15,3) = 0.5
    case(417)
       w(3,1) = 0.5
       w(3,3) = 0.5
       w(4,1) = 0.5
       w(4,3) = 0.5
       w(5,1) = 0.5
       w(5,2) = 0.5
       w(6,1) = 0.5
       w(6,2) = 0.5
       w(7,2) = 0.5
       w(7,3) = 0.5
       w(8,2) = 0.5
       w(8,3) = 0.5
       w(9,2) = 0.5
       w(9,3) = 0.5
       w(10,2) = 0.5
       w(10,3) = 0.5
       w(11,1) = 0.5
       w(11,2) = 0.5
       w(12,1) = 0.5
       w(12,2) = 0.5
       w(13,1) = 0.5
       w(13,3) = 0.5
       w(14,1) = 0.5
       w(14,3) = 0.5
    case(418)
       w(3,3) = 0.5
       w(4,3) = 0.5
       w(7,3) = 0.5
       w(8,3) = 0.5
       w(9,1) = 0.5
       w(9,2) = 0.5
       w(10,1) = 0.5
       w(10,2) = 0.5
       w(11,1) = 0.5
       w(11,2) = 0.5
       w(11,3) = 0.5
       w(12,1) = 0.5
       w(12,2) = 0.5
       w(12,3) = 0.5
       w(13,1) = 0.5
       w(13,2) = 0.5
       w(14,1) = 0.5
       w(14,2) = 0.5
       w(15,1) = 0.5
       w(15,2) = 0.5
       w(15,3) = 0.5
       w(16,1) = 0.5
       w(16,2) = 0.5
       w(16,3) = 0.5
    case(419)
       w(3,1) = 0.5
       w(3,2) = 0.5
       w(3,3) = 0.5
       w(4,1) = 0.5
       w(4,2) = 0.5
       w(4,3) = 0.5
       w(7,1) = 0.5
       w(7,2) = 0.5
       w(7,3) = 0.5
       w(8,1) = 0.5
       w(8,2) = 0.5
       w(8,3) = 0.5
       w(9,1) = 0.5
       w(9,2) = 0.5
       w(9,3) = 0.5
       w(10,1) = 0.5
       w(10,2) = 0.5
       w(10,3) = 0.5
       w(13,1) = 0.5
       w(13,2) = 0.5
       w(13,3) = 0.5
       w(14,1) = 0.5
       w(14,2) = 0.5
       w(14,3) = 0.5
    case(420)
       w(2,1) = 0.5
       w(2,2) = 0.5
       w(2,3) = 0.5
       w(4,1) = 0.5
       w(4,2) = 0.5
       w(4,3) = 0.5
       w(5,1) = 0.5
       w(5,2) = 0.5
       w(5,3) = 0.5
       w(7,1) = 0.5
       w(7,2) = 0.5
       w(7,3) = 0.5
       w(9,1) = 0.5
       w(9,2) = 0.5
       w(9,3) = 0.5
       w(11,1) = 0.5
       w(11,2) = 0.5
       w(11,3) = 0.5
       w(14,1) = 0.5
       w(14,2) = 0.5
       w(14,3) = 0.5
       w(16,1) = 0.5
       w(16,2) = 0.5
       w(16,3) = 0.5
    case(421)
       w(3,1) = 0.5
       w(3,3) = 0.5
       w(4,1) = 0.5
       w(4,3) = 0.5
       w(5,1) = 0.5
       w(5,2) = 0.5
       w(6,1) = 0.5
       w(6,2) = 0.5
       w(7,2) = 0.5
       w(7,3) = 0.5
       w(8,2) = 0.5
       w(8,3) = 0.5
       w(9,1) = 0.5
       w(10,1) = 0.5
       w(11,3) = 0.5
       w(12,3) = 0.5
       w(13,2) = 0.5
       w(14,2) = 0.5
       w(15,1) = 0.5
       w(15,2) = 0.5
       w(15,3) = 0.5
       w(16,1) = 0.5
       w(16,2) = 0.5
       w(16,3) = 0.5
    case(422)
       w(2,1) = 0.5
       w(2,2) = 0.5
       w(2,3) = 0.5
       w(4,1) = 0.5
       w(4,2) = 0.5
       w(4,3) = 0.5
       w(5,1) = 0.5
       w(5,2) = 0.5
       w(6,3) = 0.5
       w(7,1) = 0.5
       w(7,2) = 0.5
       w(8,3) = 0.5
       w(9,1) = 0.5
       w(9,2) = 0.5
       w(9,3) = 0.5
       w(11,1) = 0.5
       w(11,2) = 0.5
       w(11,3) = 0.5
       w(13,3) = 0.5
       w(14,1) = 0.5
       w(14,2) = 0.5
       w(15,3) = 0.5
       w(16,1) = 0.5
       w(16,2) = 0.5
    case(423)
       w(3,1) = 0.5
       w(3,3) = 0.5
       w(4,1) = 0.5
       w(4,3) = 0.5
       w(5,1) = 0.5
       w(5,2) = 0.5
       w(6,1) = 0.5
       w(6,2) = 0.5
       w(7,2) = 0.5
       w(7,3) = 0.5
       w(8,2) = 0.5
       w(8,3) = 0.5
       w(9,1) = 0.5
       w(9,3) = 0.5
       w(10,1) = 0.5
       w(10,3) = 0.5
       w(13,2) = 0.5
       w(13,3) = 0.5
       w(14,2) = 0.5
       w(14,3) = 0.5
       w(15,1) = 0.5
       w(15,2) = 0.5
       w(16,1) = 0.5
       w(16,2) = 0.5
    case(425)
       w(9,3) = 0.5
       w(10,3) = 0.5
       w(11,3) = 0.5
       w(12,3) = 0.5
       w(13,3) = 0.5
       w(14,3) = 0.5
       w(15,3) = 0.5
       w(16,3) = 0.5
    case(426)
       w(2,2) = 0.5
       w(2,3) = 0.25
       w(3,1) = 0.5
       w(3,2) = 0.5
       w(3,3) = 0.5
       w(4,1) = 0.5
       w(4,3) = 0.75
       w(5,2) = 0.5
       w(5,3) = 0.25
       w(7,1) = 0.5
       w(7,3) = 0.75
       w(8,1) = 0.5
       w(8,2) = 0.5
       w(8,3) = 0.5
       w(9,2) = 0.5
       w(9,3) = 0.25
       w(11,1) = 0.5
       w(11,3) = 0.75
       w(12,1) = 0.5
       w(12,2) = 0.5
       w(12,3) = 0.5
       w(14,2) = 0.5
       w(14,3) = 0.25
       w(15,1) = 0.5
       w(15,2) = 0.5
       w(15,3) = 0.5
       w(16,1) = 0.5
       w(16,3) = 0.75
    case(427)
       w(3,1) = 0.25
       w(3,2) = 0.75
       w(3,3) = 0.25
       w(4,1) = 0.25
       w(4,2) = 0.75
       w(4,3) = 0.25
       w(5,1) = 0.5
       w(5,3) = 0.5
       w(6,1) = 0.5
       w(6,3) = 0.5
       w(7,1) = 0.25
       w(7,2) = 0.25
       w(7,3) = 0.75
       w(8,1) = 0.25
       w(8,2) = 0.25
       w(8,3) = 0.75
       w(11,1) = 0.25
       w(11,2) = 0.25
       w(11,3) = 0.75
       w(12,1) = 0.25
       w(12,2) = 0.25
       w(12,3) = 0.75
       w(13,1) = 0.5
       w(13,3) = 0.5
       w(14,1) = 0.5
       w(14,3) = 0.5
       w(15,1) = 0.25
       w(15,2) = 0.75
       w(15,3) = 0.25
       w(16,1) = 0.25
       w(16,2) = 0.75
       w(16,3) = 0.25
    case(428)
       w(2,2) = 0.5
       w(2,3) = 0.25
       w(3,1) = 0.5
       w(3,2) = 0.5
       w(3,3) = 0.5
       w(4,1) = 0.5
       w(4,3) = 0.75
       w(5,1) = 0.5
       w(5,3) = 0.25
       w(6,1) = 0.5
       w(6,2) = 0.5
       w(7,2) = 0.5
       w(7,3) = 0.75
       w(8,3) = 0.5
       w(9,2) = 0.5
       w(9,3) = 0.25
       w(11,1) = 0.5
       w(11,3) = 0.75
       w(12,1) = 0.5
       w(12,2) = 0.5
       w(12,3) = 0.5
       w(13,1) = 0.5
       w(13,2) = 0.5
       w(14,1) = 0.5
       w(14,3) = 0.25
       w(15,3) = 0.5
       w(16,2) = 0.5
       w(16,3) = 0.75
    case(429)
       w(3,1) = 0.25
       w(3,2) = 0.75
       w(3,3) = 0.25
       w(4,1) = 0.25
       w(4,2) = 0.75
       w(4,3) = 0.25
       w(5,1) = 0.5
       w(5,3) = 0.5
       w(6,1) = 0.5
       w(6,3) = 0.5
       w(7,1) = 0.25
       w(7,2) = 0.25
       w(7,3) = 0.75
       w(8,1) = 0.25
       w(8,2) = 0.25
       w(8,3) = 0.75
       w(9,3) = 0.5
       w(10,3) = 0.5
       w(11,1) = 0.25
       w(11,2) = 0.25
       w(11,3) = 0.25
       w(12,1) = 0.25
       w(12,2) = 0.25
       w(12,3) = 0.25
       w(13,1) = 0.5
       w(14,1) = 0.5
       w(15,1) = 0.25
       w(15,2) = 0.75
       w(15,3) = 0.75
       w(16,1) = 0.25
       w(16,2) = 0.75
       w(16,3) = 0.75

    case default
       steer(20) = 1
    end select
    return

  end subroutine getwyckoff

end module hall
