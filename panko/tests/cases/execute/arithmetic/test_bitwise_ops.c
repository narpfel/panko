// [[nosnapshot]]

// i8
// [[arg: %hhd\n]]
// i16
// [[arg: %hd\n]]
// i32
// [[arg: %d\n]]
// i64
// [[arg: %ld\n]]
// u8
// [[arg: %hhu\n]]
// u16
// [[arg: %hu\n]]
// u32
// [[arg: %u\n]]
// u64
// [[arg: %lu\n]]

int printf(char const*, ...);

int main(int, char** argv) {
    // i8 & i32 => i32
    {
        char signed lhs = 116;
        int rhs = -644076986;
        int res = lhs & rhs;
        // [[print: 68]]
        printf(*(argv + 3), res);
    }
    {
        char signed lhs = -86;
        int rhs = -122987932;
        int res = lhs & rhs;
        // [[print: -122988000]]
        printf(*(argv + 3), res);
    }
    {
        char signed lhs = -67;
        int rhs = -1513894816;
        int res = lhs & rhs;
        // [[print: -1513894880]]
        printf(*(argv + 3), res);
    }
    {
        char signed lhs = -12;
        int rhs = 1344638428;
        int res = lhs & rhs;
        // [[print: 1344638420]]
        printf(*(argv + 3), res);
    }
    {
        char signed lhs = -91;
        int rhs = 715533316;
        int res = lhs & rhs;
        // [[print: 715533316]]
        printf(*(argv + 3), res);
    }
    {
        char signed lhs = 3;
        int rhs = 1492349417;
        int res = lhs & rhs;
        // [[print: 1]]
        printf(*(argv + 3), res);
    }
    {
        char signed lhs = 118;
        int rhs = -777476575;
        int res = lhs & rhs;
        // [[print: 32]]
        printf(*(argv + 3), res);
    }
    {
        char signed lhs = -88;
        int rhs = 1250498716;
        int res = lhs & rhs;
        // [[print: 1250498696]]
        printf(*(argv + 3), res);
    }
    {
        char signed lhs = -54;
        int rhs = 719341648;
        int res = lhs & rhs;
        // [[print: 719341632]]
        printf(*(argv + 3), res);
    }
    {
        char signed lhs = -29;
        int rhs = 1043660815;
        int res = lhs & rhs;
        // [[print: 1043660803]]
        printf(*(argv + 3), res);
    }

    // i8 ^ i32 => i32
    {
        char signed lhs = 49;
        int rhs = 1767738799;
        int res = lhs ^ rhs;
        // [[print: 1767738782]]
        printf(*(argv + 3), res);
    }
    {
        char signed lhs = 30;
        int rhs = 1229084770;
        int res = lhs ^ rhs;
        // [[print: 1229084796]]
        printf(*(argv + 3), res);
    }
    {
        char signed lhs = -25;
        int rhs = 505388847;
        int res = lhs ^ rhs;
        // [[print: -505388856]]
        printf(*(argv + 3), res);
    }
    {
        char signed lhs = 15;
        int rhs = -1610482274;
        int res = lhs ^ rhs;
        // [[print: -1610482287]]
        printf(*(argv + 3), res);
    }
    {
        char signed lhs = -112;
        int rhs = -2039487280;
        int res = lhs ^ rhs;
        // [[print: 2039487296]]
        printf(*(argv + 3), res);
    }
    {
        char signed lhs = -113;
        int rhs = 359589488;
        int res = lhs ^ rhs;
        // [[print: -359589377]]
        printf(*(argv + 3), res);
    }
    {
        char signed lhs = -106;
        int rhs = 1235369109;
        int res = lhs ^ rhs;
        // [[print: -1235369213]]
        printf(*(argv + 3), res);
    }
    {
        char signed lhs = 51;
        int rhs = 758284395;
        int res = lhs ^ rhs;
        // [[print: 758284376]]
        printf(*(argv + 3), res);
    }
    {
        char signed lhs = 73;
        int rhs = 1198015330;
        int res = lhs ^ rhs;
        // [[print: 1198015275]]
        printf(*(argv + 3), res);
    }
    {
        char signed lhs = 32;
        int rhs = -535043568;
        int res = lhs ^ rhs;
        // [[print: -535043536]]
        printf(*(argv + 3), res);
    }

    // i8 | i32 => i32
    {
        char signed lhs = -15;
        int rhs = -832304797;
        int res = lhs | rhs;
        // [[print: -13]]
        printf(*(argv + 3), res);
    }
    {
        char signed lhs = 84;
        int rhs = -73373623;
        int res = lhs | rhs;
        // [[print: -73373603]]
        printf(*(argv + 3), res);
    }
    {
        char signed lhs = -30;
        int rhs = 226588596;
        int res = lhs | rhs;
        // [[print: -10]]
        printf(*(argv + 3), res);
    }
    {
        char signed lhs = 50;
        int rhs = -427320251;
        int res = lhs | rhs;
        // [[print: -427320201]]
        printf(*(argv + 3), res);
    }
    {
        char signed lhs = 105;
        int rhs = -1389632439;
        int res = lhs | rhs;
        // [[print: -1389632407]]
        printf(*(argv + 3), res);
    }
    {
        char signed lhs = 11;
        int rhs = 1126343066;
        int res = lhs | rhs;
        // [[print: 1126343067]]
        printf(*(argv + 3), res);
    }
    {
        char signed lhs = 100;
        int rhs = -962881772;
        int res = lhs | rhs;
        // [[print: -962881676]]
        printf(*(argv + 3), res);
    }
    {
        char signed lhs = 99;
        int rhs = 786300855;
        int res = lhs | rhs;
        // [[print: 786300919]]
        printf(*(argv + 3), res);
    }
    {
        char signed lhs = 83;
        int rhs = -1160023646;
        int res = lhs | rhs;
        // [[print: -1160023565]]
        printf(*(argv + 3), res);
    }
    {
        char signed lhs = 27;
        int rhs = 353225689;
        int res = lhs | rhs;
        // [[print: 353225691]]
        printf(*(argv + 3), res);
    }

    // i32 & i8 => i32
    {
        int lhs = -1468641316;
        char signed rhs = -74;
        int res = lhs & rhs;
        // [[print: -1468641388]]
        printf(*(argv + 3), res);
    }
    {
        int lhs = 1659322647;
        char signed rhs = 53;
        int res = lhs & rhs;
        // [[print: 21]]
        printf(*(argv + 3), res);
    }
    {
        int lhs = 1808831912;
        char signed rhs = -90;
        int res = lhs & rhs;
        // [[print: 1808831904]]
        printf(*(argv + 3), res);
    }
    {
        int lhs = 1690821086;
        char signed rhs = 94;
        int res = lhs & rhs;
        // [[print: 94]]
        printf(*(argv + 3), res);
    }
    {
        int lhs = -1201273570;
        char signed rhs = 72;
        int res = lhs & rhs;
        // [[print: 8]]
        printf(*(argv + 3), res);
    }
    {
        int lhs = -1154023998;
        char signed rhs = -72;
        int res = lhs & rhs;
        // [[print: -1154024064]]
        printf(*(argv + 3), res);
    }
    {
        int lhs = -1867388722;
        char signed rhs = 69;
        int res = lhs & rhs;
        // [[print: 68]]
        printf(*(argv + 3), res);
    }
    {
        int lhs = -1658845847;
        char signed rhs = -86;
        int res = lhs & rhs;
        // [[print: -1658845912]]
        printf(*(argv + 3), res);
    }
    {
        int lhs = 1410520152;
        char signed rhs = 4;
        int res = lhs & rhs;
        // [[print: 0]]
        printf(*(argv + 3), res);
    }
    {
        int lhs = -2119943857;
        char signed rhs = -45;
        int res = lhs & rhs;
        // [[print: -2119943869]]
        printf(*(argv + 3), res);
    }

    // i32 ^ i8 => i32
    {
        int lhs = -125590812;
        char signed rhs = 82;
        int res = lhs ^ rhs;
        // [[print: -125590858]]
        printf(*(argv + 3), res);
    }
    {
        int lhs = 1544520148;
        char signed rhs = -63;
        int res = lhs ^ rhs;
        // [[print: -1544520171]]
        printf(*(argv + 3), res);
    }
    {
        int lhs = 215128728;
        char signed rhs = -76;
        int res = lhs ^ rhs;
        // [[print: -215128788]]
        printf(*(argv + 3), res);
    }
    {
        int lhs = -2039900127;
        char signed rhs = 20;
        int res = lhs ^ rhs;
        // [[print: -2039900107]]
        printf(*(argv + 3), res);
    }
    {
        int lhs = -298817216;
        char signed rhs = -3;
        int res = lhs ^ rhs;
        // [[print: 298817213]]
        printf(*(argv + 3), res);
    }
    {
        int lhs = -2087133535;
        char signed rhs = -101;
        int res = lhs ^ rhs;
        // [[print: 2087133498]]
        printf(*(argv + 3), res);
    }
    {
        int lhs = 114714620;
        char signed rhs = 112;
        int res = lhs ^ rhs;
        // [[print: 114714508]]
        printf(*(argv + 3), res);
    }
    {
        int lhs = -1856979233;
        char signed rhs = 120;
        int res = lhs ^ rhs;
        // [[print: -1856979289]]
        printf(*(argv + 3), res);
    }
    {
        int lhs = 1380042708;
        char signed rhs = -101;
        int res = lhs ^ rhs;
        // [[print: -1380042673]]
        printf(*(argv + 3), res);
    }
    {
        int lhs = -1405611725;
        char signed rhs = 64;
        int res = lhs ^ rhs;
        // [[print: -1405611661]]
        printf(*(argv + 3), res);
    }

    // i32 | i8 => i32
    {
        int lhs = -1290532451;
        char signed rhs = 60;
        int res = lhs | rhs;
        // [[print: -1290532419]]
        printf(*(argv + 3), res);
    }
    {
        int lhs = 482565084;
        char signed rhs = -38;
        int res = lhs | rhs;
        // [[print: -34]]
        printf(*(argv + 3), res);
    }
    {
        int lhs = 1621412376;
        char signed rhs = 49;
        int res = lhs | rhs;
        // [[print: 1621412409]]
        printf(*(argv + 3), res);
    }
    {
        int lhs = -1510220810;
        char signed rhs = -108;
        int res = lhs | rhs;
        // [[print: -10]]
        printf(*(argv + 3), res);
    }
    {
        int lhs = -1104551586;
        char signed rhs = 34;
        int res = lhs | rhs;
        // [[print: -1104551554]]
        printf(*(argv + 3), res);
    }
    {
        int lhs = -124181640;
        char signed rhs = -12;
        int res = lhs | rhs;
        // [[print: -4]]
        printf(*(argv + 3), res);
    }
    {
        int lhs = -2130863163;
        char signed rhs = 15;
        int res = lhs | rhs;
        // [[print: -2130863153]]
        printf(*(argv + 3), res);
    }
    {
        int lhs = 1374855335;
        char signed rhs = -75;
        int res = lhs | rhs;
        // [[print: -73]]
        printf(*(argv + 3), res);
    }
    {
        int lhs = 720152189;
        char signed rhs = 118;
        int res = lhs | rhs;
        // [[print: 720152191]]
        printf(*(argv + 3), res);
    }
    {
        int lhs = -23671321;
        char signed rhs = 43;
        int res = lhs | rhs;
        // [[print: -23671313]]
        printf(*(argv + 3), res);
    }

    // u8 & u32 => u32
    {
        char unsigned lhs = 48;
        unsigned int rhs = 4240541053u;
        unsigned int res = lhs & rhs;
        // [[print: 48]]
        printf(*(argv + 7), res);
    }
    {
        char unsigned lhs = 79;
        unsigned int rhs = 335615598u;
        unsigned int res = lhs & rhs;
        // [[print: 78]]
        printf(*(argv + 7), res);
    }
    {
        char unsigned lhs = 76;
        unsigned int rhs = 1956389656u;
        unsigned int res = lhs & rhs;
        // [[print: 8]]
        printf(*(argv + 7), res);
    }
    {
        char unsigned lhs = 88;
        unsigned int rhs = 613510983u;
        unsigned int res = lhs & rhs;
        // [[print: 64]]
        printf(*(argv + 7), res);
    }
    {
        char unsigned lhs = 26;
        unsigned int rhs = 3676590156u;
        unsigned int res = lhs & rhs;
        // [[print: 8]]
        printf(*(argv + 7), res);
    }
    {
        char unsigned lhs = 175;
        unsigned int rhs = 1394110u;
        unsigned int res = lhs & rhs;
        // [[print: 174]]
        printf(*(argv + 7), res);
    }
    {
        char unsigned lhs = 193;
        unsigned int rhs = 2265978118u;
        unsigned int res = lhs & rhs;
        // [[print: 0]]
        printf(*(argv + 7), res);
    }
    {
        char unsigned lhs = 120;
        unsigned int rhs = 1790294941u;
        unsigned int res = lhs & rhs;
        // [[print: 24]]
        printf(*(argv + 7), res);
    }
    {
        char unsigned lhs = 66;
        unsigned int rhs = 3864545697u;
        unsigned int res = lhs & rhs;
        // [[print: 0]]
        printf(*(argv + 7), res);
    }
    {
        char unsigned lhs = 246;
        unsigned int rhs = 2488084182u;
        unsigned int res = lhs & rhs;
        // [[print: 214]]
        printf(*(argv + 7), res);
    }

    // u8 ^ u32 => u32
    {
        char unsigned lhs = 154;
        unsigned int rhs = 924560703u;
        unsigned int res = lhs ^ rhs;
        // [[print: 924560805]]
        printf(*(argv + 7), res);
    }
    {
        char unsigned lhs = 19;
        unsigned int rhs = 1485747121u;
        unsigned int res = lhs ^ rhs;
        // [[print: 1485747106]]
        printf(*(argv + 7), res);
    }
    {
        char unsigned lhs = 240;
        unsigned int rhs = 3105925521u;
        unsigned int res = lhs ^ rhs;
        // [[print: 3105925473]]
        printf(*(argv + 7), res);
    }
    {
        char unsigned lhs = 233;
        unsigned int rhs = 214305238u;
        unsigned int res = lhs ^ rhs;
        // [[print: 214305087]]
        printf(*(argv + 7), res);
    }
    {
        char unsigned lhs = 251;
        unsigned int rhs = 2727058473u;
        unsigned int res = lhs ^ rhs;
        // [[print: 2727058642]]
        printf(*(argv + 7), res);
    }
    {
        char unsigned lhs = 184;
        unsigned int rhs = 988734253u;
        unsigned int res = lhs ^ rhs;
        // [[print: 988734357]]
        printf(*(argv + 7), res);
    }
    {
        char unsigned lhs = 87;
        unsigned int rhs = 3037025938u;
        unsigned int res = lhs ^ rhs;
        // [[print: 3037025989]]
        printf(*(argv + 7), res);
    }
    {
        char unsigned lhs = 55;
        unsigned int rhs = 3722175923u;
        unsigned int res = lhs ^ rhs;
        // [[print: 3722175876]]
        printf(*(argv + 7), res);
    }
    {
        char unsigned lhs = 15;
        unsigned int rhs = 2329494253u;
        unsigned int res = lhs ^ rhs;
        // [[print: 2329494242]]
        printf(*(argv + 7), res);
    }
    {
        char unsigned lhs = 71;
        unsigned int rhs = 4182724484u;
        unsigned int res = lhs ^ rhs;
        // [[print: 4182724547]]
        printf(*(argv + 7), res);
    }

    // u8 | u32 => u32
    {
        char unsigned lhs = 74;
        unsigned int rhs = 517912121u;
        unsigned int res = lhs | rhs;
        // [[print: 517912187]]
        printf(*(argv + 7), res);
    }
    {
        char unsigned lhs = 98;
        unsigned int rhs = 3862948212u;
        unsigned int res = lhs | rhs;
        // [[print: 3862948214]]
        printf(*(argv + 7), res);
    }
    {
        char unsigned lhs = 64;
        unsigned int rhs = 3083574784u;
        unsigned int res = lhs | rhs;
        // [[print: 3083574848]]
        printf(*(argv + 7), res);
    }
    {
        char unsigned lhs = 236;
        unsigned int rhs = 4009438041u;
        unsigned int res = lhs | rhs;
        // [[print: 4009438205]]
        printf(*(argv + 7), res);
    }
    {
        char unsigned lhs = 17;
        unsigned int rhs = 3567517532u;
        unsigned int res = lhs | rhs;
        // [[print: 3567517533]]
        printf(*(argv + 7), res);
    }
    {
        char unsigned lhs = 194;
        unsigned int rhs = 2863635115u;
        unsigned int res = lhs | rhs;
        // [[print: 2863635179]]
        printf(*(argv + 7), res);
    }
    {
        char unsigned lhs = 129;
        unsigned int rhs = 3888195989u;
        unsigned int res = lhs | rhs;
        // [[print: 3888195989]]
        printf(*(argv + 7), res);
    }
    {
        char unsigned lhs = 161;
        unsigned int rhs = 3312954942u;
        unsigned int res = lhs | rhs;
        // [[print: 3312955071]]
        printf(*(argv + 7), res);
    }
    {
        char unsigned lhs = 232;
        unsigned int rhs = 3632294207u;
        unsigned int res = lhs | rhs;
        // [[print: 3632294399]]
        printf(*(argv + 7), res);
    }
    {
        char unsigned lhs = 5;
        unsigned int rhs = 4283478248u;
        unsigned int res = lhs | rhs;
        // [[print: 4283478253]]
        printf(*(argv + 7), res);
    }

    // u32 & u8 => u32
    {
        unsigned int lhs = 909520274u;
        char unsigned rhs = 30;
        unsigned int res = lhs & rhs;
        // [[print: 18]]
        printf(*(argv + 7), res);
    }
    {
        unsigned int lhs = 3307903518u;
        char unsigned rhs = 17;
        unsigned int res = lhs & rhs;
        // [[print: 16]]
        printf(*(argv + 7), res);
    }
    {
        unsigned int lhs = 2333764209u;
        char unsigned rhs = 220;
        unsigned int res = lhs & rhs;
        // [[print: 80]]
        printf(*(argv + 7), res);
    }
    {
        unsigned int lhs = 3557182376u;
        char unsigned rhs = 16;
        unsigned int res = lhs & rhs;
        // [[print: 0]]
        printf(*(argv + 7), res);
    }
    {
        unsigned int lhs = 2348069617u;
        char unsigned rhs = 224;
        unsigned int res = lhs & rhs;
        // [[print: 224]]
        printf(*(argv + 7), res);
    }
    {
        unsigned int lhs = 1488136553u;
        char unsigned rhs = 56;
        unsigned int res = lhs & rhs;
        // [[print: 40]]
        printf(*(argv + 7), res);
    }
    {
        unsigned int lhs = 1608653877u;
        char unsigned rhs = 210;
        unsigned int res = lhs & rhs;
        // [[print: 16]]
        printf(*(argv + 7), res);
    }
    {
        unsigned int lhs = 2659671978u;
        char unsigned rhs = 130;
        unsigned int res = lhs & rhs;
        // [[print: 130]]
        printf(*(argv + 7), res);
    }
    {
        unsigned int lhs = 2894416506u;
        char unsigned rhs = 114;
        unsigned int res = lhs & rhs;
        // [[print: 114]]
        printf(*(argv + 7), res);
    }
    {
        unsigned int lhs = 1374674980u;
        char unsigned rhs = 7;
        unsigned int res = lhs & rhs;
        // [[print: 4]]
        printf(*(argv + 7), res);
    }

    // u32 ^ u8 => u32
    {
        unsigned int lhs = 3279755309u;
        char unsigned rhs = 168;
        unsigned int res = lhs ^ rhs;
        // [[print: 3279755397]]
        printf(*(argv + 7), res);
    }
    {
        unsigned int lhs = 3167386619u;
        char unsigned rhs = 157;
        unsigned int res = lhs ^ rhs;
        // [[print: 3167386470]]
        printf(*(argv + 7), res);
    }
    {
        unsigned int lhs = 2001299910u;
        char unsigned rhs = 58;
        unsigned int res = lhs ^ rhs;
        // [[print: 2001299964]]
        printf(*(argv + 7), res);
    }
    {
        unsigned int lhs = 2897827445u;
        char unsigned rhs = 32;
        unsigned int res = lhs ^ rhs;
        // [[print: 2897827413]]
        printf(*(argv + 7), res);
    }
    {
        unsigned int lhs = 827694118u;
        char unsigned rhs = 94;
        unsigned int res = lhs ^ rhs;
        // [[print: 827694200]]
        printf(*(argv + 7), res);
    }
    {
        unsigned int lhs = 2564773506u;
        char unsigned rhs = 227;
        unsigned int res = lhs ^ rhs;
        // [[print: 2564773473]]
        printf(*(argv + 7), res);
    }
    {
        unsigned int lhs = 2756926784u;
        char unsigned rhs = 222;
        unsigned int res = lhs ^ rhs;
        // [[print: 2756926878]]
        printf(*(argv + 7), res);
    }
    {
        unsigned int lhs = 1326611245u;
        char unsigned rhs = 2;
        unsigned int res = lhs ^ rhs;
        // [[print: 1326611247]]
        printf(*(argv + 7), res);
    }
    {
        unsigned int lhs = 1487314812u;
        char unsigned rhs = 16;
        unsigned int res = lhs ^ rhs;
        // [[print: 1487314796]]
        printf(*(argv + 7), res);
    }
    {
        unsigned int lhs = 3826410395u;
        char unsigned rhs = 114;
        unsigned int res = lhs ^ rhs;
        // [[print: 3826410473]]
        printf(*(argv + 7), res);
    }

    // u32 | u8 => u32
    {
        unsigned int lhs = 2343989490u;
        char unsigned rhs = 215;
        unsigned int res = lhs | rhs;
        // [[print: 2343989495]]
        printf(*(argv + 7), res);
    }
    {
        unsigned int lhs = 308688965u;
        char unsigned rhs = 189;
        unsigned int res = lhs | rhs;
        // [[print: 308689149]]
        printf(*(argv + 7), res);
    }
    {
        unsigned int lhs = 373861484u;
        char unsigned rhs = 94;
        unsigned int res = lhs | rhs;
        // [[print: 373861502]]
        printf(*(argv + 7), res);
    }
    {
        unsigned int lhs = 236363971u;
        char unsigned rhs = 131;
        unsigned int res = lhs | rhs;
        // [[print: 236363971]]
        printf(*(argv + 7), res);
    }
    {
        unsigned int lhs = 2395766529u;
        char unsigned rhs = 37;
        unsigned int res = lhs | rhs;
        // [[print: 2395766565]]
        printf(*(argv + 7), res);
    }
    {
        unsigned int lhs = 233112196u;
        char unsigned rhs = 82;
        unsigned int res = lhs | rhs;
        // [[print: 233112278]]
        printf(*(argv + 7), res);
    }
    {
        unsigned int lhs = 680599466u;
        char unsigned rhs = 2;
        unsigned int res = lhs | rhs;
        // [[print: 680599466]]
        printf(*(argv + 7), res);
    }
    {
        unsigned int lhs = 4027135075u;
        char unsigned rhs = 136;
        unsigned int res = lhs | rhs;
        // [[print: 4027135211]]
        printf(*(argv + 7), res);
    }
    {
        unsigned int lhs = 1451090394u;
        char unsigned rhs = 236;
        unsigned int res = lhs | rhs;
        // [[print: 1451090430]]
        printf(*(argv + 7), res);
    }
    {
        unsigned int lhs = 4095661145u;
        char unsigned rhs = 220;
        unsigned int res = lhs | rhs;
        // [[print: 4095661277]]
        printf(*(argv + 7), res);
    }

    // i32 & i32 => i32
    {
        int lhs = 649162039;
        int rhs = -428943752;
        int res = lhs & rhs;
        // [[print: 639648816]]
        printf(*(argv + 3), res);
    }
    {
        int lhs = -1717051292;
        int rhs = 1699047673;
        int res = lhs & rhs;
        // [[print: 17129568]]
        printf(*(argv + 3), res);
    }
    {
        int lhs = 695684918;
        int rhs = 417275771;
        int res = lhs & rhs;
        // [[print: 139923250]]
        printf(*(argv + 3), res);
    }
    {
        int lhs = -621423227;
        int rhs = -1599854434;
        int res = lhs & rhs;
        // [[print: -2136735612]]
        printf(*(argv + 3), res);
    }
    {
        int lhs = 1000504586;
        int rhs = -1625419434;
        int res = lhs & rhs;
        // [[print: 453120258]]
        printf(*(argv + 3), res);
    }
    {
        int lhs = -1281151700;
        int rhs = 932599029;
        int res = lhs & rhs;
        // [[print: 864158756]]
        printf(*(argv + 3), res);
    }
    {
        int lhs = -1615496102;
        int rhs = -2044171503;
        int res = lhs & rhs;
        // [[print: -2044696560]]
        printf(*(argv + 3), res);
    }
    {
        int lhs = 1573727595;
        int rhs = -1256011680;
        int res = lhs & rhs;
        // [[print: 352323680]]
        printf(*(argv + 3), res);
    }
    {
        int lhs = -1760352324;
        int rhs = -1503292780;
        int res = lhs & rhs;
        // [[print: -2046754156]]
        printf(*(argv + 3), res);
    }
    {
        int lhs = 21911479;
        int rhs = -920133066;
        int res = lhs & rhs;
        // [[print: 17187382]]
        printf(*(argv + 3), res);
    }

    // i32 ^ i32 => i32
    {
        int lhs = -1810200023;
        int rhs = 1816379723;
        int res = lhs ^ rhs;
        // [[print: -128365726]]
        printf(*(argv + 3), res);
    }
    {
        int lhs = 505117171;
        int rhs = 800753533;
        int res = lhs ^ rhs;
        // [[print: 832700046]]
        printf(*(argv + 3), res);
    }
    {
        int lhs = 1939260420;
        int rhs = 1034030933;
        int res = lhs ^ rhs;
        // [[print: 1312082769]]
        printf(*(argv + 3), res);
    }
    {
        int lhs = -533399186;
        int rhs = 1931666099;
        int res = lhs ^ rhs;
        // [[print: -1827267619]]
        printf(*(argv + 3), res);
    }
    {
        int lhs = -2139352899;
        int rhs = 1656414054;
        int res = lhs ^ rhs;
        // [[print: -490287141]]
        printf(*(argv + 3), res);
    }
    {
        int lhs = 1012156780;
        int rhs = 405565652;
        int res = lhs ^ rhs;
        // [[print: 611858872]]
        printf(*(argv + 3), res);
    }
    {
        int lhs = -1168090496;
        int rhs = 532065026;
        int res = lhs ^ rhs;
        // [[print: -1512640126]]
        printf(*(argv + 3), res);
    }
    {
        int lhs = -106571883;
        int rhs = 1286802732;
        int res = lhs ^ rhs;
        // [[print: -1256793415]]
        printf(*(argv + 3), res);
    }
    {
        int lhs = 254151627;
        int rhs = 1123094172;
        int res = lhs ^ rhs;
        // [[print: 1305937239]]
        printf(*(argv + 3), res);
    }
    {
        int lhs = 1423354451;
        int rhs = 284228580;
        int res = lhs ^ rhs;
        // [[print: 1143361975]]
        printf(*(argv + 3), res);
    }

    // i32 | i32 => i32
    {
        int lhs = 363068404;
        int rhs = -920494798;
        int res = lhs | rhs;
        // [[print: -576454666]]
        printf(*(argv + 3), res);
    }
    {
        int lhs = 861825865;
        int rhs = -651117473;
        int res = lhs | rhs;
        // [[print: -75563169]]
        printf(*(argv + 3), res);
    }
    {
        int lhs = -22656878;
        int rhs = 1405297379;
        int res = lhs | rhs;
        // [[print: -1611021]]
        printf(*(argv + 3), res);
    }
    {
        int lhs = 1189444861;
        int rhs = -2105433699;
        int res = lhs | rhs;
        // [[print: -958005763]]
        printf(*(argv + 3), res);
    }
    {
        int lhs = 1266742113;
        int rhs = 1870389388;
        int res = lhs | rhs;
        // [[print: 1878782957]]
        printf(*(argv + 3), res);
    }
    {
        int lhs = -856145449;
        int rhs = -1408696587;
        int res = lhs | rhs;
        // [[print: -319225865]]
        printf(*(argv + 3), res);
    }
    {
        int lhs = 1042035350;
        int rhs = 832534018;
        int res = lhs | rhs;
        // [[print: 1067415190]]
        printf(*(argv + 3), res);
    }
    {
        int lhs = -1033633179;
        int rhs = 2085101342;
        int res = lhs | rhs;
        // [[print: -26468481]]
        printf(*(argv + 3), res);
    }
    {
        int lhs = -1466741194;
        int rhs = -1179506234;
        int res = lhs | rhs;
        // [[print: -1179422730]]
        printf(*(argv + 3), res);
    }
    {
        int lhs = -761846286;
        int rhs = -931837740;
        int res = lhs | rhs;
        // [[print: -621318666]]
        printf(*(argv + 3), res);
    }

    // u32 & u32 => u32
    {
        unsigned int lhs = 3844545095u;
        unsigned int rhs = 2745379493u;
        unsigned int res = lhs & rhs;
        // [[print: 2703428101]]
        printf(*(argv + 7), res);
    }
    {
        unsigned int lhs = 1007690911u;
        unsigned int rhs = 3123861425u;
        unsigned int res = lhs & rhs;
        // [[print: 940573841]]
        printf(*(argv + 7), res);
    }
    {
        unsigned int lhs = 1353570211u;
        unsigned int rhs = 1824549392u;
        unsigned int res = lhs & rhs;
        // [[print: 1082147328]]
        printf(*(argv + 7), res);
    }
    {
        unsigned int lhs = 1460818184u;
        unsigned int rhs = 1834250896u;
        unsigned int res = lhs & rhs;
        // [[print: 1158696960]]
        printf(*(argv + 7), res);
    }
    {
        unsigned int lhs = 2023819100u;
        unsigned int rhs = 2727330817u;
        unsigned int res = lhs & rhs;
        // [[print: 545326080]]
        printf(*(argv + 7), res);
    }
    {
        unsigned int lhs = 2046623804u;
        unsigned int rhs = 2501272736u;
        unsigned int res = lhs & rhs;
        // [[print: 286523424]]
        printf(*(argv + 7), res);
    }
    {
        unsigned int lhs = 115002781u;
        unsigned int rhs = 2402657189u;
        unsigned int res = lhs & rhs;
        // [[print: 101745029]]
        printf(*(argv + 7), res);
    }
    {
        unsigned int lhs = 2108345962u;
        unsigned int rhs = 1291173981u;
        unsigned int res = lhs & rhs;
        // [[print: 1285603400]]
        printf(*(argv + 7), res);
    }
    {
        unsigned int lhs = 43518703u;
        unsigned int rhs = 1302018744u;
        unsigned int res = lhs & rhs;
        // [[print: 9964200]]
        printf(*(argv + 7), res);
    }
    {
        unsigned int lhs = 2792387185u;
        unsigned int rhs = 1169554935u;
        unsigned int res = lhs & rhs;
        // [[print: 70283377]]
        printf(*(argv + 7), res);
    }

    // u32 ^ u32 => u32
    {
        unsigned int lhs = 1791494094u;
        unsigned int rhs = 3695200969u;
        unsigned int res = lhs ^ rhs;
        // [[print: 3062385927]]
        printf(*(argv + 7), res);
    }
    {
        unsigned int lhs = 3858710553u;
        unsigned int rhs = 4097734118u;
        unsigned int res = lhs ^ rhs;
        // [[print: 297874943]]
        printf(*(argv + 7), res);
    }
    {
        unsigned int lhs = 2995159634u;
        unsigned int rhs = 3973395101u;
        unsigned int res = lhs ^ rhs;
        // [[print: 1582543055]]
        printf(*(argv + 7), res);
    }
    {
        unsigned int lhs = 769334243u;
        unsigned int rhs = 3138678711u;
        unsigned int res = lhs ^ rhs;
        // [[print: 2530178132]]
        printf(*(argv + 7), res);
    }
    {
        unsigned int lhs = 2083384106u;
        unsigned int rhs = 3676717908u;
        unsigned int res = lhs ^ rhs;
        // [[print: 2802559102]]
        printf(*(argv + 7), res);
    }
    {
        unsigned int lhs = 3968187447u;
        unsigned int rhs = 2323522798u;
        unsigned int res = lhs ^ rhs;
        // [[print: 1727764697]]
        printf(*(argv + 7), res);
    }
    {
        unsigned int lhs = 1911114181u;
        unsigned int rhs = 3480747158u;
        unsigned int res = lhs ^ rhs;
        // [[print: 3198072147]]
        printf(*(argv + 7), res);
    }
    {
        unsigned int lhs = 3274304484u;
        unsigned int rhs = 3748721044u;
        unsigned int res = lhs ^ rhs;
        // [[print: 475601520]]
        printf(*(argv + 7), res);
    }
    {
        unsigned int lhs = 398068064u;
        unsigned int rhs = 2201211156u;
        unsigned int res = lhs ^ rhs;
        // [[print: 2492061812]]
        printf(*(argv + 7), res);
    }
    {
        unsigned int lhs = 2679776379u;
        unsigned int rhs = 1351402577u;
        unsigned int res = lhs ^ rhs;
        // [[print: 3476480042]]
        printf(*(argv + 7), res);
    }

    // u32 | u32 => u32
    {
        unsigned int lhs = 3211033851u;
        unsigned int rhs = 2389466142u;
        unsigned int res = lhs | rhs;
        // [[print: 3211558143]]
        printf(*(argv + 7), res);
    }
    {
        unsigned int lhs = 4082884966u;
        unsigned int rhs = 1865728078u;
        unsigned int res = lhs | rhs;
        // [[print: 4286570862]]
        printf(*(argv + 7), res);
    }
    {
        unsigned int lhs = 1187575561u;
        unsigned int rhs = 2935998104u;
        unsigned int res = lhs | rhs;
        // [[print: 4009752473]]
        printf(*(argv + 7), res);
    }
    {
        unsigned int lhs = 2103675262u;
        unsigned int rhs = 3268524019u;
        unsigned int res = lhs | rhs;
        // [[print: 4294164479]]
        printf(*(argv + 7), res);
    }
    {
        unsigned int lhs = 382718211u;
        unsigned int rhs = 2909215477u;
        unsigned int res = lhs | rhs;
        // [[print: 3220167671]]
        printf(*(argv + 7), res);
    }
    {
        unsigned int lhs = 3283073605u;
        unsigned int rhs = 2741630761u;
        unsigned int res = lhs | rhs;
        // [[print: 3824156525]]
        printf(*(argv + 7), res);
    }
    {
        unsigned int lhs = 12519185u;
        unsigned int rhs = 2786786343u;
        unsigned int res = lhs | rhs;
        // [[print: 2797600567]]
        printf(*(argv + 7), res);
    }
    {
        unsigned int lhs = 367284797u;
        unsigned int rhs = 3220073344u;
        unsigned int res = lhs | rhs;
        // [[print: 3220077501]]
        printf(*(argv + 7), res);
    }
    {
        unsigned int lhs = 540395882u;
        unsigned int rhs = 1116003253u;
        unsigned int res = lhs | rhs;
        // [[print: 1656085503]]
        printf(*(argv + 7), res);
    }
    {
        unsigned int lhs = 735048556u;
        unsigned int rhs = 4055711834u;
        unsigned int res = lhs | rhs;
        // [[print: 4227855230]]
        printf(*(argv + 7), res);
    }

    // u8 & i32 => i32
    {
        char unsigned lhs = 101;
        int rhs = -211732657;
        int res = lhs & rhs;
        // [[print: 69]]
        printf(*(argv + 3), res);
    }
    {
        char unsigned lhs = 194;
        int rhs = -2085233983;
        int res = lhs & rhs;
        // [[print: 192]]
        printf(*(argv + 3), res);
    }
    {
        char unsigned lhs = 7;
        int rhs = 202721816;
        int res = lhs & rhs;
        // [[print: 0]]
        printf(*(argv + 3), res);
    }
    {
        char unsigned lhs = 31;
        int rhs = 1054637416;
        int res = lhs & rhs;
        // [[print: 8]]
        printf(*(argv + 3), res);
    }
    {
        char unsigned lhs = 62;
        int rhs = -815806413;
        int res = lhs & rhs;
        // [[print: 50]]
        printf(*(argv + 3), res);
    }
    {
        char unsigned lhs = 163;
        int rhs = -1610890717;
        int res = lhs & rhs;
        // [[print: 35]]
        printf(*(argv + 3), res);
    }
    {
        char unsigned lhs = 186;
        int rhs = -1987781760;
        int res = lhs & rhs;
        // [[print: 128]]
        printf(*(argv + 3), res);
    }
    {
        char unsigned lhs = 10;
        int rhs = -1348021976;
        int res = lhs & rhs;
        // [[print: 8]]
        printf(*(argv + 3), res);
    }
    {
        char unsigned lhs = 141;
        int rhs = -2059533539;
        int res = lhs & rhs;
        // [[print: 13]]
        printf(*(argv + 3), res);
    }
    {
        char unsigned lhs = 28;
        int rhs = 2006001699;
        int res = lhs & rhs;
        // [[print: 0]]
        printf(*(argv + 3), res);
    }

    // u8 ^ i32 => i32
    {
        char unsigned lhs = 83;
        int rhs = 1103699340;
        int res = lhs ^ rhs;
        // [[print: 1103699423]]
        printf(*(argv + 3), res);
    }
    {
        char unsigned lhs = 240;
        int rhs = 2083534557;
        int res = lhs ^ rhs;
        // [[print: 2083534381]]
        printf(*(argv + 3), res);
    }
    {
        char unsigned lhs = 66;
        int rhs = 1552142680;
        int res = lhs ^ rhs;
        // [[print: 1552142618]]
        printf(*(argv + 3), res);
    }
    {
        char unsigned lhs = 199;
        int rhs = 652774721;
        int res = lhs ^ rhs;
        // [[print: 652774790]]
        printf(*(argv + 3), res);
    }
    {
        char unsigned lhs = 215;
        int rhs = 1961414368;
        int res = lhs ^ rhs;
        // [[print: 1961414199]]
        printf(*(argv + 3), res);
    }
    {
        char unsigned lhs = 247;
        int rhs = 963777557;
        int res = lhs ^ rhs;
        // [[print: 963777762]]
        printf(*(argv + 3), res);
    }
    {
        char unsigned lhs = 196;
        int rhs = -1906472913;
        int res = lhs ^ rhs;
        // [[print: -1906472725]]
        printf(*(argv + 3), res);
    }
    {
        char unsigned lhs = 221;
        int rhs = 62642445;
        int res = lhs ^ rhs;
        // [[print: 62642640]]
        printf(*(argv + 3), res);
    }
    {
        char unsigned lhs = 83;
        int rhs = -1809942476;
        int res = lhs ^ rhs;
        // [[print: -1809942425]]
        printf(*(argv + 3), res);
    }
    {
        char unsigned lhs = 140;
        int rhs = 646335560;
        int res = lhs ^ rhs;
        // [[print: 646335684]]
        printf(*(argv + 3), res);
    }

    // u8 | i32 => i32
    {
        char unsigned lhs = 137;
        int rhs = 49042231;
        int res = lhs | rhs;
        // [[print: 49042367]]
        printf(*(argv + 3), res);
    }
    {
        char unsigned lhs = 47;
        int rhs = 285656544;
        int res = lhs | rhs;
        // [[print: 285656559]]
        printf(*(argv + 3), res);
    }
    {
        char unsigned lhs = 117;
        int rhs = -514250038;
        int res = lhs | rhs;
        // [[print: -514249985]]
        printf(*(argv + 3), res);
    }
    {
        char unsigned lhs = 158;
        int rhs = -1744245263;
        int res = lhs | rhs;
        // [[print: -1744245249]]
        printf(*(argv + 3), res);
    }
    {
        char unsigned lhs = 255;
        int rhs = 203892943;
        int res = lhs | rhs;
        // [[print: 203892991]]
        printf(*(argv + 3), res);
    }
    {
        char unsigned lhs = 37;
        int rhs = 50406424;
        int res = lhs | rhs;
        // [[print: 50406461]]
        printf(*(argv + 3), res);
    }
    {
        char unsigned lhs = 24;
        int rhs = 2079659390;
        int res = lhs | rhs;
        // [[print: 2079659390]]
        printf(*(argv + 3), res);
    }
    {
        char unsigned lhs = 212;
        int rhs = -1738330526;
        int res = lhs | rhs;
        // [[print: -1738330378]]
        printf(*(argv + 3), res);
    }
    {
        char unsigned lhs = 32;
        int rhs = 1880266078;
        int res = lhs | rhs;
        // [[print: 1880266110]]
        printf(*(argv + 3), res);
    }
    {
        char unsigned lhs = 254;
        int rhs = -2044422565;
        int res = lhs | rhs;
        // [[print: -2044422401]]
        printf(*(argv + 3), res);
    }

    // i32 & u8 => i32
    {
        int lhs = 2038989530;
        char unsigned rhs = 178;
        int res = lhs & rhs;
        // [[print: 146]]
        printf(*(argv + 3), res);
    }
    {
        int lhs = 2113384553;
        char unsigned rhs = 196;
        int res = lhs & rhs;
        // [[print: 64]]
        printf(*(argv + 3), res);
    }
    {
        int lhs = -792884896;
        char unsigned rhs = 108;
        int res = lhs & rhs;
        // [[print: 96]]
        printf(*(argv + 3), res);
    }
    {
        int lhs = -1561836170;
        char unsigned rhs = 90;
        int res = lhs & rhs;
        // [[print: 82]]
        printf(*(argv + 3), res);
    }
    {
        int lhs = 492548721;
        char unsigned rhs = 226;
        int res = lhs & rhs;
        // [[print: 96]]
        printf(*(argv + 3), res);
    }
    {
        int lhs = 2042367346;
        char unsigned rhs = 171;
        int res = lhs & rhs;
        // [[print: 34]]
        printf(*(argv + 3), res);
    }
    {
        int lhs = 251353276;
        char unsigned rhs = 137;
        int res = lhs & rhs;
        // [[print: 136]]
        printf(*(argv + 3), res);
    }
    {
        int lhs = -1620509141;
        char unsigned rhs = 28;
        int res = lhs & rhs;
        // [[print: 8]]
        printf(*(argv + 3), res);
    }
    {
        int lhs = -1057452045;
        char unsigned rhs = 11;
        int res = lhs & rhs;
        // [[print: 3]]
        printf(*(argv + 3), res);
    }
    {
        int lhs = 839520425;
        char unsigned rhs = 21;
        int res = lhs & rhs;
        // [[print: 1]]
        printf(*(argv + 3), res);
    }

    // i32 ^ u8 => i32
    {
        int lhs = 748890686;
        char unsigned rhs = 46;
        int res = lhs ^ rhs;
        // [[print: 748890640]]
        printf(*(argv + 3), res);
    }
    {
        int lhs = -2063995164;
        char unsigned rhs = 116;
        int res = lhs ^ rhs;
        // [[print: -2063995248]]
        printf(*(argv + 3), res);
    }
    {
        int lhs = -383336414;
        char unsigned rhs = 241;
        int res = lhs ^ rhs;
        // [[print: -383336237]]
        printf(*(argv + 3), res);
    }
    {
        int lhs = 499517287;
        char unsigned rhs = 230;
        int res = lhs ^ rhs;
        // [[print: 499517313]]
        printf(*(argv + 3), res);
    }
    {
        int lhs = 2146020248;
        char unsigned rhs = 72;
        int res = lhs ^ rhs;
        // [[print: 2146020304]]
        printf(*(argv + 3), res);
    }
    {
        int lhs = 1590184549;
        char unsigned rhs = 98;
        int res = lhs ^ rhs;
        // [[print: 1590184455]]
        printf(*(argv + 3), res);
    }
    {
        int lhs = 1876760167;
        char unsigned rhs = 126;
        int res = lhs ^ rhs;
        // [[print: 1876760089]]
        printf(*(argv + 3), res);
    }
    {
        int lhs = -769234373;
        char unsigned rhs = 95;
        int res = lhs ^ rhs;
        // [[print: -769234332]]
        printf(*(argv + 3), res);
    }
    {
        int lhs = -1407569278;
        char unsigned rhs = 185;
        int res = lhs ^ rhs;
        // [[print: -1407569349]]
        printf(*(argv + 3), res);
    }
    {
        int lhs = 771235301;
        char unsigned rhs = 30;
        int res = lhs ^ rhs;
        // [[print: 771235323]]
        printf(*(argv + 3), res);
    }

    // i32 | u8 => i32
    {
        int lhs = 1658750213;
        char unsigned rhs = 65;
        int res = lhs | rhs;
        // [[print: 1658750277]]
        printf(*(argv + 3), res);
    }
    {
        int lhs = 980429165;
        char unsigned rhs = 91;
        int res = lhs | rhs;
        // [[print: 980429183]]
        printf(*(argv + 3), res);
    }
    {
        int lhs = -664709337;
        char unsigned rhs = 105;
        int res = lhs | rhs;
        // [[print: -664709265]]
        printf(*(argv + 3), res);
    }
    {
        int lhs = 2139940791;
        char unsigned rhs = 233;
        int res = lhs | rhs;
        // [[print: 2139940863]]
        printf(*(argv + 3), res);
    }
    {
        int lhs = -956664898;
        char unsigned rhs = 28;
        int res = lhs | rhs;
        // [[print: -956664898]]
        printf(*(argv + 3), res);
    }
    {
        int lhs = -2033659184;
        char unsigned rhs = 93;
        int res = lhs | rhs;
        // [[print: -2033659171]]
        printf(*(argv + 3), res);
    }
    {
        int lhs = 1622497654;
        char unsigned rhs = 202;
        int res = lhs | rhs;
        // [[print: 1622497790]]
        printf(*(argv + 3), res);
    }
    {
        int lhs = 1873762440;
        char unsigned rhs = 73;
        int res = lhs | rhs;
        // [[print: 1873762505]]
        printf(*(argv + 3), res);
    }
    {
        int lhs = 641845719;
        char unsigned rhs = 28;
        int res = lhs | rhs;
        // [[print: 641845727]]
        printf(*(argv + 3), res);
    }
    {
        int lhs = 991655573;
        char unsigned rhs = 82;
        int res = lhs | rhs;
        // [[print: 991655639]]
        printf(*(argv + 3), res);
    }

    // i64 & i64 => i64
    {
        long lhs = 2801925621546384929l;
        long rhs = 7230552429529602570l;
        long res = lhs & rhs;
        // [[print: 2612105376653515264]]
        printf(*(argv + 4), res);
    }
    {
        long lhs = -7439079844207318580l;
        long rhs = -3537418311052460117l;
        long res = lhs & rhs;
        // [[print: -8592850181118940792]]
        printf(*(argv + 4), res);
    }
    {
        long lhs = 2551941021867334818l;
        long rhs = 8473860497076483970l;
        long res = lhs & rhs;
        // [[print: 2380170066120692866]]
        printf(*(argv + 4), res);
    }
    {
        long lhs = 1538810968095846084l;
        long rhs = 1657983943462225734l;
        long res = lhs & rhs;
        // [[print: 1513864835959490116]]
        printf(*(argv + 4), res);
    }
    {
        long lhs = -223622610014358405l;
        long rhs = 51858354609635308l;
        long res = lhs & rhs;
        // [[print: 45045213340644456]]
        printf(*(argv + 4), res);
    }
    {
        long lhs = 6464711645432396533l;
        long rhs = 161815921028837262l;
        long res = lhs & rhs;
        // [[print: 15270812076747396]]
        printf(*(argv + 4), res);
    }
    {
        long lhs = -8572933024662424032l;
        long rhs = 5045626401963901980l;
        long res = lhs & rhs;
        // [[print: 1269183442846720]]
        printf(*(argv + 4), res);
    }
    {
        long lhs = -4844159035895159217l;
        long rhs = -8716657978372743497l;
        long res = lhs & rhs;
        // [[print: -8935118570413619705]]
        printf(*(argv + 4), res);
    }
    {
        long lhs = -4734579690529004167l;
        long rhs = -4272294446490126915l;
        long res = lhs & rhs;
        // [[print: -8934788914484058823]]
        printf(*(argv + 4), res);
    }
    {
        long lhs = -1267623414582930609l;
        long rhs = 6535012005171379209l;
        long res = lhs & rhs;
        // [[print: 5341275791370031113]]
        printf(*(argv + 4), res);
    }

    // i64 ^ i64 => i64
    {
        long lhs = -7043486557509728637l;
        long rhs = 3173227751842189260l;
        long res = lhs ^ rhs;
        // [[print: -5599923114536089265]]
        printf(*(argv + 4), res);
    }
    {
        long lhs = -7351452775450336577l;
        long rhs = -1019528723272189253l;
        long res = lhs ^ rhs;
        // [[print: 7503996029985134596]]
        printf(*(argv + 4), res);
    }
    {
        long lhs = 2683619796050460749l;
        long rhs = -8602189625432203969l;
        long res = lhs ^ rhs;
        // [[print: -5935528938773455502]]
        printf(*(argv + 4), res);
    }
    {
        long lhs = -7385801902280962739l;
        long rhs = 2821434749860598328l;
        long res = lhs ^ rhs;
        // [[print: -4708544497804255371]]
        printf(*(argv + 4), res);
    }
    {
        long lhs = 3102828786036920003l;
        long rhs = 7045107565011606749l;
        long res = lhs ^ rhs;
        // [[print: 5389179654051025438]]
        printf(*(argv + 4), res);
    }
    {
        long lhs = -7933991599608262890l;
        long rhs = -5989728202377255198l;
        long res = lhs ^ rhs;
        // [[print: 4396913199329356276]]
        printf(*(argv + 4), res);
    }
    {
        long lhs = -8822541651606602609l;
        long rhs = 6096772933325456300l;
        long res = lhs ^ rhs;
        // [[print: -3383294367191859421]]
        printf(*(argv + 4), res);
    }
    {
        long lhs = -6201572462228236371l;
        long rhs = -6438734885167380891l;
        long res = lhs ^ rhs;
        // [[print: 1101853654830567880]]
        printf(*(argv + 4), res);
    }
    {
        long lhs = -4510426432251458647l;
        long rhs = -7888596032638999000l;
        long res = lhs ^ rhs;
        // [[print: 6044302092219297153]]
        printf(*(argv + 4), res);
    }
    {
        long lhs = 1105524543073234509l;
        long rhs = -2031464034377387509l;
        long res = lhs ^ rhs;
        // [[print: -1397991034445685690]]
        printf(*(argv + 4), res);
    }

    // i64 | i64 => i64
    {
        long lhs = -546049263944809602l;
        long rhs = -6013802227964930590l;
        long res = lhs | rhs;
        // [[print: -221046099023570946]]
        printf(*(argv + 4), res);
    }
    {
        long lhs = 6567627937983591804l;
        long rhs = -6795305552173117462l;
        long res = lhs | rhs;
        // [[print: -308778210672541698]]
        printf(*(argv + 4), res);
    }
    {
        long lhs = 6709534871078707759l;
        long rhs = -7366446183872771352l;
        long res = lhs | rhs;
        // [[print: -2459758712575792401]]
        printf(*(argv + 4), res);
    }
    {
        long lhs = -4580511988279384175l;
        long rhs = -2511296126848901319l;
        long res = lhs | rhs;
        // [[print: -2490818248912961607]]
        printf(*(argv + 4), res);
    }
    {
        long lhs = 5608494342923665022l;
        long rhs = -6560768997669557795l;
        long res = lhs | rhs;
        // [[print: -1299429247520948225]]
        printf(*(argv + 4), res);
    }
    {
        long lhs = -2942324281370765685l;
        long rhs = -7359998854092083527l;
        long res = lhs | rhs;
        // [[print: -2306182003551125829]]
        printf(*(argv + 4), res);
    }
    {
        long lhs = 5888465965911346724l;
        long rhs = -9069475834077805365l;
        long res = lhs | rhs;
        // [[print: -3190026417331007761]]
        printf(*(argv + 4), res);
    }
    {
        long lhs = -8221548605618582257l;
        long rhs = 644582004372533054l;
        long res = lhs | rhs;
        // [[print: -8217041842126983361]]
        printf(*(argv + 4), res);
    }
    {
        long lhs = -6462048859213964165l;
        long rhs = -3368399969265334061l;
        long res = lhs | rhs;
        // [[print: -625091190134022917]]
        printf(*(argv + 4), res);
    }
    {
        long lhs = -1579075438726447040l;
        long rhs = -5909490996253488325l;
        long res = lhs | rhs;
        // [[print: -1153485005423670405]]
        printf(*(argv + 4), res);
    }

    // u64 & u64 => u64
    {
        unsigned long lhs = 16746232955359325215ul;
        unsigned long rhs = 10488716663740416741ul;
        unsigned long res = lhs & rhs;
        // [[print: 9225061031705952261]]
        printf(*(argv + 8), res);
    }
    {
        unsigned long lhs = 6791879479299731126ul;
        unsigned long rhs = 244527773852882292ul;
        unsigned long res = lhs & rhs;
        // [[print: 162296878919369780]]
        printf(*(argv + 8), res);
    }
    {
        unsigned long lhs = 7328404248969567941ul;
        unsigned long rhs = 12347843725370396928ul;
        unsigned long res = lhs & rhs;
        // [[print: 2382430943614406656]]
        printf(*(argv + 8), res);
    }
    {
        unsigned long lhs = 1875987558484514241ul;
        unsigned long rhs = 818178153266705115ul;
        unsigned long res = lhs & rhs;
        // [[print: 723039611063898305]]
        printf(*(argv + 8), res);
    }
    {
        unsigned long lhs = 17059747401580490897ul;
        unsigned long rhs = 3932117625028579792ul;
        unsigned long res = lhs & rhs;
        // [[print: 2630137504262590608]]
        printf(*(argv + 8), res);
    }
    {
        unsigned long lhs = 7240817562203833964ul;
        unsigned long rhs = 12122200968884367157ul;
        unsigned long res = lhs & rhs;
        // [[print: 2321751860506101284]]
        printf(*(argv + 8), res);
    }
    {
        unsigned long lhs = 7095793349701257486ul;
        unsigned long rhs = 4387921879311083084ul;
        unsigned long res = lhs & rhs;
        // [[print: 2333148488486895628]]
        printf(*(argv + 8), res);
    }
    {
        unsigned long lhs = 15035132872370915746ul;
        unsigned long rhs = 14935902890838945884ul;
        unsigned long res = lhs & rhs;
        // [[print: 13836892590435663872]]
        printf(*(argv + 8), res);
    }
    {
        unsigned long lhs = 6455083621281712170ul;
        unsigned long rhs = 11363914624762419906ul;
        unsigned long res = lhs & rhs;
        // [[print: 1843115739701053442]]
        printf(*(argv + 8), res);
    }
    {
        unsigned long lhs = 12725034075836716223ul;
        unsigned long rhs = 12228137813559759049ul;
        unsigned long res = lhs & rhs;
        // [[print: 11569749642824388745]]
        printf(*(argv + 8), res);
    }

    // u64 ^ u64 => u64
    {
        unsigned long lhs = 1011778242459051724ul;
        unsigned long rhs = 9893598595878736402ul;
        unsigned long res = lhs ^ rhs;
        // [[print: 9747919838564218078]]
        printf(*(argv + 8), res);
    }
    {
        unsigned long lhs = 13831423283889071065ul;
        unsigned long rhs = 533634704115361024ul;
        unsigned long res = lhs ^ rhs;
        // [[print: 13300484153882150617]]
        printf(*(argv + 8), res);
    }
    {
        unsigned long lhs = 12229004064580946020ul;
        unsigned long rhs = 205649962591958568ul;
        unsigned long res = lhs ^ rhs;
        // [[print: 12352454494661541452]]
        printf(*(argv + 8), res);
    }
    {
        unsigned long lhs = 15973028215589447979ul;
        unsigned long rhs = 3778530264089516740ul;
        unsigned long res = lhs ^ rhs;
        // [[print: 16851222727466316783]]
        printf(*(argv + 8), res);
    }
    {
        unsigned long lhs = 10087376055606001997ul;
        unsigned long rhs = 5990777583630158082ul;
        unsigned long res = lhs ^ rhs;
        // [[print: 15626940311321397327]]
        printf(*(argv + 8), res);
    }
    {
        unsigned long lhs = 8675658663980168237ul;
        unsigned long rhs = 190317935012273251ul;
        unsigned long res = lhs ^ rhs;
        // [[print: 8845637778878834766]]
        printf(*(argv + 8), res);
    }
    {
        unsigned long lhs = 16583607952180264523ul;
        unsigned long rhs = 367601287774213327ul;
        unsigned long res = lhs ^ rhs;
        // [[print: 16374302085393296004]]
        printf(*(argv + 8), res);
    }
    {
        unsigned long lhs = 14359577516908269161ul;
        unsigned long rhs = 7113648021755667283ul;
        unsigned long res = lhs ^ rhs;
        // [[print: 11961479815483682106]]
        printf(*(argv + 8), res);
    }
    {
        unsigned long lhs = 16124713965755348919ul;
        unsigned long rhs = 7191430311740139956ul;
        unsigned long res = lhs ^ rhs;
        // [[print: 13550040654635064835]]
        printf(*(argv + 8), res);
    }
    {
        unsigned long lhs = 6936619737850163716ul;
        unsigned long rhs = 6495865142417359474ul;
        unsigned long res = lhs ^ rhs;
        // [[print: 4208086122336923766]]
        printf(*(argv + 8), res);
    }

    // u64 | u64 => u64
    {
        unsigned long lhs = 9731497658150290152ul;
        unsigned long rhs = 3270325051402223845ul;
        unsigned long res = lhs | rhs;
        // [[print: 12641533579109335789]]
        printf(*(argv + 8), res);
    }
    {
        unsigned long lhs = 16395411660374731094ul;
        unsigned long rhs = 9624705113705674403ul;
        unsigned long res = lhs | rhs;
        // [[print: 16688640717034094583]]
        printf(*(argv + 8), res);
    }
    {
        unsigned long lhs = 4274010172279580104ul;
        unsigned long rhs = 272697716228453800ul;
        unsigned long res = lhs | rhs;
        // [[print: 4312431646369084904]]
        printf(*(argv + 8), res);
    }
    {
        unsigned long lhs = 14475369295508989200ul;
        unsigned long rhs = 4957325725930432534ul;
        unsigned long res = lhs | rhs;
        // [[print: 14766168480872787222]]
        printf(*(argv + 8), res);
    }
    {
        unsigned long lhs = 4959153889853639984ul;
        unsigned long rhs = 2408494112713652430ul;
        unsigned long res = lhs | rhs;
        // [[print: 7349580689403788798]]
        printf(*(argv + 8), res);
    }
    {
        unsigned long lhs = 18384490164830475436ul;
        unsigned long rhs = 4245998781129748507ul;
        unsigned long res = lhs | rhs;
        // [[print: 18441912164567151807]]
        printf(*(argv + 8), res);
    }
    {
        unsigned long lhs = 5235548208220242733ul;
        unsigned long rhs = 9003649790042994034ul;
        unsigned long res = lhs | rhs;
        // [[print: 9005906091559548799]]
        printf(*(argv + 8), res);
    }
    {
        unsigned long lhs = 8298964657946175873ul;
        unsigned long rhs = 3564304675359761870ul;
        unsigned long res = lhs | rhs;
        // [[print: 8322648228704497103]]
        printf(*(argv + 8), res);
    }
    {
        unsigned long lhs = 13900592547836559626ul;
        unsigned long rhs = 11173042144128225085ul;
        unsigned long res = lhs | rhs;
        // [[print: 15847850326242611007]]
        printf(*(argv + 8), res);
    }
    {
        unsigned long lhs = 243836602903393063ul;
        unsigned long rhs = 5111297805974315368ul;
        unsigned long res = lhs | rhs;
        // [[print: 5183355507940750191]]
        printf(*(argv + 8), res);
    }

}
