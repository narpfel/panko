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
    // i8 * i32 => i32
    {
        char signed lhs = 74;
        int rhs = 1095810364;
        int res = lhs * rhs;
        // [[print: -514411688]]
        printf(*(argv + 3), res);
    }
    {
        char signed lhs = -39;
        int rhs = 220952599;
        int res = lhs * rhs;
        // [[print: -27216769]]
        printf(*(argv + 3), res);
    }
    {
        char signed lhs = -24;
        int rhs = 1085561991;
        int res = lhs * rhs;
        // [[print: -283684008]]
        printf(*(argv + 3), res);
    }
    {
        char signed lhs = 31;
        int rhs = 535356770;
        int res = lhs * rhs;
        // [[print: -583809314]]
        printf(*(argv + 3), res);
    }
    {
        char signed lhs = -38;
        int rhs = 335489304;
        int res = lhs * rhs;
        // [[print: 136308336]]
        printf(*(argv + 3), res);
    }
    {
        char signed lhs = -50;
        int rhs = -200379493;
        int res = lhs * rhs;
        // [[print: 1429040058]]
        printf(*(argv + 3), res);
    }
    {
        char signed lhs = -79;
        int rhs = 1063100910;
        int res = lhs * rhs;
        // [[print: 1914374030]]
        printf(*(argv + 3), res);
    }
    {
        char signed lhs = -43;
        int rhs = 1433830872;
        int res = lhs * rhs;
        // [[print: -1525185352]]
        printf(*(argv + 3), res);
    }
    {
        char signed lhs = -59;
        int rhs = -1565065243;
        int res = lhs * rhs;
        // [[print: 2144536121]]
        printf(*(argv + 3), res);
    }
    {
        char signed lhs = 7;
        int rhs = 1314541822;
        int res = lhs * rhs;
        // [[print: 611858162]]
        printf(*(argv + 3), res);
    }

    // i8 / i32 => i32
    {
        char signed lhs = 22;
        int rhs = 2130723222;
        int res = lhs / rhs;
        // [[print: 0]]
        printf(*(argv + 3), res);
    }
    {
        char signed lhs = 20;
        int rhs = -526338596;
        int res = lhs / rhs;
        // [[print: 0]]
        printf(*(argv + 3), res);
    }
    {
        char signed lhs = 124;
        int rhs = -1624152800;
        int res = lhs / rhs;
        // [[print: 0]]
        printf(*(argv + 3), res);
    }
    {
        char signed lhs = 3;
        int rhs = -386789464;
        int res = lhs / rhs;
        // [[print: 0]]
        printf(*(argv + 3), res);
    }
    {
        char signed lhs = 85;
        int rhs = -1178379746;
        int res = lhs / rhs;
        // [[print: 0]]
        printf(*(argv + 3), res);
    }
    {
        char signed lhs = 117;
        int rhs = 1133021937;
        int res = lhs / rhs;
        // [[print: 0]]
        printf(*(argv + 3), res);
    }
    {
        char signed lhs = -88;
        int rhs = -765693828;
        int res = lhs / rhs;
        // [[print: 0]]
        printf(*(argv + 3), res);
    }
    {
        char signed lhs = 16;
        int rhs = 75762238;
        int res = lhs / rhs;
        // [[print: 0]]
        printf(*(argv + 3), res);
    }
    {
        char signed lhs = -113;
        int rhs = 1362494507;
        int res = lhs / rhs;
        // [[print: 0]]
        printf(*(argv + 3), res);
    }
    {
        char signed lhs = 72;
        int rhs = 1218267917;
        int res = lhs / rhs;
        // [[print: 0]]
        printf(*(argv + 3), res);
    }

    // i8 % i32 => i32
    {
        char signed lhs = -85;
        int rhs = -1617910420;
        int res = lhs % rhs;
        // [[print: -85]]
        printf(*(argv + 3), res);
    }
    {
        char signed lhs = -61;
        int rhs = -2140800499;
        int res = lhs % rhs;
        // [[print: -61]]
        printf(*(argv + 3), res);
    }
    {
        char signed lhs = 44;
        int rhs = 520461062;
        int res = lhs % rhs;
        // [[print: 44]]
        printf(*(argv + 3), res);
    }
    {
        char signed lhs = -26;
        int rhs = -648125865;
        int res = lhs % rhs;
        // [[print: -26]]
        printf(*(argv + 3), res);
    }
    {
        char signed lhs = 83;
        int rhs = 1008864711;
        int res = lhs % rhs;
        // [[print: 83]]
        printf(*(argv + 3), res);
    }
    {
        char signed lhs = 110;
        int rhs = -2047720568;
        int res = lhs % rhs;
        // [[print: 110]]
        printf(*(argv + 3), res);
    }
    {
        char signed lhs = 64;
        int rhs = -1334554146;
        int res = lhs % rhs;
        // [[print: 64]]
        printf(*(argv + 3), res);
    }
    {
        char signed lhs = -43;
        int rhs = -432510322;
        int res = lhs % rhs;
        // [[print: -43]]
        printf(*(argv + 3), res);
    }
    {
        char signed lhs = -51;
        int rhs = 318297158;
        int res = lhs % rhs;
        // [[print: -51]]
        printf(*(argv + 3), res);
    }
    {
        char signed lhs = 116;
        int rhs = 1264634008;
        int res = lhs % rhs;
        // [[print: 116]]
        printf(*(argv + 3), res);
    }

    // i32 * i8 => i32
    {
        int lhs = 1241245592;
        char signed rhs = -9;
        int res = lhs * rhs;
        // [[print: 1713691560]]
        printf(*(argv + 3), res);
    }
    {
        int lhs = -1404875231;
        char signed rhs = -85;
        int res = lhs * rhs;
        // [[print: -844689653]]
        printf(*(argv + 3), res);
    }
    {
        int lhs = -2034668286;
        char signed rhs = 78;
        int res = lhs * rhs;
        // [[print: 209663644]]
        printf(*(argv + 3), res);
    }
    {
        int lhs = -1534926537;
        char signed rhs = -112;
        int res = lhs * rhs;
        // [[print: 113080304]]
        printf(*(argv + 3), res);
    }
    {
        int lhs = 991799229;
        char signed rhs = 15;
        int res = lhs * rhs;
        // [[print: 1992086547]]
        printf(*(argv + 3), res);
    }
    {
        int lhs = -1571135137;
        char signed rhs = -3;
        int res = lhs * rhs;
        // [[print: 418438115]]
        printf(*(argv + 3), res);
    }
    {
        int lhs = 1039078959;
        char signed rhs = 12;
        int res = lhs * rhs;
        // [[print: -415954380]]
        printf(*(argv + 3), res);
    }
    {
        int lhs = 417462966;
        char signed rhs = -8;
        int res = lhs * rhs;
        // [[print: 955263568]]
        printf(*(argv + 3), res);
    }
    {
        int lhs = 1643922295;
        char signed rhs = -30;
        int res = lhs * rhs;
        // [[print: -2073028594]]
        printf(*(argv + 3), res);
    }
    {
        int lhs = 651307866;
        char signed rhs = 121;
        int res = lhs * rhs;
        // [[print: 1498840458]]
        printf(*(argv + 3), res);
    }

    // i32 / i8 => i32
    {
        int lhs = -996391813;
        char signed rhs = -70;
        int res = lhs / rhs;
        // [[print: 14234168]]
        printf(*(argv + 3), res);
    }
    {
        int lhs = -1905165617;
        char signed rhs = -30;
        int res = lhs / rhs;
        // [[print: 63505520]]
        printf(*(argv + 3), res);
    }
    {
        int lhs = -82884916;
        char signed rhs = -90;
        int res = lhs / rhs;
        // [[print: 920943]]
        printf(*(argv + 3), res);
    }
    {
        int lhs = 821756556;
        char signed rhs = -98;
        int res = lhs / rhs;
        // [[print: -8385270]]
        printf(*(argv + 3), res);
    }
    {
        int lhs = -1066721212;
        char signed rhs = 58;
        int res = lhs / rhs;
        // [[print: -18391745]]
        printf(*(argv + 3), res);
    }
    {
        int lhs = -2098581132;
        char signed rhs = -89;
        int res = lhs / rhs;
        // [[print: 23579563]]
        printf(*(argv + 3), res);
    }
    {
        int lhs = 1259711911;
        char signed rhs = -116;
        int res = lhs / rhs;
        // [[print: -10859585]]
        printf(*(argv + 3), res);
    }
    {
        int lhs = -524946600;
        char signed rhs = -95;
        int res = lhs / rhs;
        // [[print: 5525753]]
        printf(*(argv + 3), res);
    }
    {
        int lhs = 1884303693;
        char signed rhs = -63;
        int res = lhs / rhs;
        // [[print: -29909582]]
        printf(*(argv + 3), res);
    }
    {
        int lhs = 1698350784;
        char signed rhs = 63;
        int res = lhs / rhs;
        // [[print: 26957948]]
        printf(*(argv + 3), res);
    }

    // i32 % i8 => i32
    {
        int lhs = -2139711910;
        char signed rhs = -22;
        int res = lhs % rhs;
        // [[print: -6]]
        printf(*(argv + 3), res);
    }
    {
        int lhs = -566489291;
        char signed rhs = -102;
        int res = lhs % rhs;
        // [[print: -59]]
        printf(*(argv + 3), res);
    }
    {
        int lhs = -344552210;
        char signed rhs = -7;
        int res = lhs % rhs;
        // [[print: -2]]
        printf(*(argv + 3), res);
    }
    {
        int lhs = 233013792;
        char signed rhs = 92;
        int res = lhs % rhs;
        // [[print: 56]]
        printf(*(argv + 3), res);
    }
    {
        int lhs = 2038675348;
        char signed rhs = -9;
        int res = lhs % rhs;
        // [[print: 1]]
        printf(*(argv + 3), res);
    }
    {
        int lhs = 1240442094;
        char signed rhs = 13;
        int res = lhs % rhs;
        // [[print: 8]]
        printf(*(argv + 3), res);
    }
    {
        int lhs = 774661860;
        char signed rhs = -46;
        int res = lhs % rhs;
        // [[print: 10]]
        printf(*(argv + 3), res);
    }
    {
        int lhs = 274143535;
        char signed rhs = 54;
        int res = lhs % rhs;
        // [[print: 7]]
        printf(*(argv + 3), res);
    }
    {
        int lhs = -143878682;
        char signed rhs = -98;
        int res = lhs % rhs;
        // [[print: -80]]
        printf(*(argv + 3), res);
    }
    {
        int lhs = 120136605;
        char signed rhs = 88;
        int res = lhs % rhs;
        // [[print: 61]]
        printf(*(argv + 3), res);
    }

    // u8 * u32 => u32
    {
        char unsigned lhs = 221;
        unsigned int rhs = 3356079800;
        unsigned int res = lhs * rhs;
        // [[print: 2959260888]]
        printf(*(argv + 7), res);
    }
    {
        char unsigned lhs = 148;
        unsigned int rhs = 2143909894;
        unsigned int res = lhs * rhs;
        // [[print: 3766051704]]
        printf(*(argv + 7), res);
    }
    {
        char unsigned lhs = 167;
        unsigned int rhs = 4055620795;
        unsigned int res = lhs * rhs;
        // [[print: 2978807293]]
        printf(*(argv + 7), res);
    }
    {
        char unsigned lhs = 72;
        unsigned int rhs = 344239587;
        unsigned int res = lhs * rhs;
        // [[print: 3310413784]]
        printf(*(argv + 7), res);
    }
    {
        char unsigned lhs = 95;
        unsigned int rhs = 3788785085;
        unsigned int res = lhs * rhs;
        // [[print: 3452297507]]
        printf(*(argv + 7), res);
    }
    {
        char unsigned lhs = 134;
        unsigned int rhs = 2389282163;
        unsigned int res = lhs * rhs;
        // [[print: 2336229938]]
        printf(*(argv + 7), res);
    }
    {
        char unsigned lhs = 186;
        unsigned int rhs = 1860696443;
        unsigned int res = lhs * rhs;
        // [[print: 2492154718]]
        printf(*(argv + 7), res);
    }
    {
        char unsigned lhs = 109;
        unsigned int rhs = 2279274733;
        unsigned int res = lhs * rhs;
        // [[print: 3627810025]]
        printf(*(argv + 7), res);
    }
    {
        char unsigned lhs = 122;
        unsigned int rhs = 2790188657;
        unsigned int res = lhs * rhs;
        // [[print: 1100599770]]
        printf(*(argv + 7), res);
    }
    {
        char unsigned lhs = 37;
        unsigned int rhs = 588755674;
        unsigned int res = lhs * rhs;
        // [[print: 309123458]]
        printf(*(argv + 7), res);
    }

    // u8 / u32 => u32
    {
        char unsigned lhs = 58;
        unsigned int rhs = 1368033447;
        unsigned int res = lhs / rhs;
        // [[print: 0]]
        printf(*(argv + 7), res);
    }
    {
        char unsigned lhs = 65;
        unsigned int rhs = 1237336755;
        unsigned int res = lhs / rhs;
        // [[print: 0]]
        printf(*(argv + 7), res);
    }
    {
        char unsigned lhs = 253;
        unsigned int rhs = 2888507760;
        unsigned int res = lhs / rhs;
        // [[print: 0]]
        printf(*(argv + 7), res);
    }
    {
        char unsigned lhs = 95;
        unsigned int rhs = 2352884500;
        unsigned int res = lhs / rhs;
        // [[print: 0]]
        printf(*(argv + 7), res);
    }
    {
        char unsigned lhs = 170;
        unsigned int rhs = 1621420794;
        unsigned int res = lhs / rhs;
        // [[print: 0]]
        printf(*(argv + 7), res);
    }
    {
        char unsigned lhs = 19;
        unsigned int rhs = 2911396732;
        unsigned int res = lhs / rhs;
        // [[print: 0]]
        printf(*(argv + 7), res);
    }
    {
        char unsigned lhs = 159;
        unsigned int rhs = 2345210453;
        unsigned int res = lhs / rhs;
        // [[print: 0]]
        printf(*(argv + 7), res);
    }
    {
        char unsigned lhs = 31;
        unsigned int rhs = 954918160;
        unsigned int res = lhs / rhs;
        // [[print: 0]]
        printf(*(argv + 7), res);
    }
    {
        char unsigned lhs = 228;
        unsigned int rhs = 1926245250;
        unsigned int res = lhs / rhs;
        // [[print: 0]]
        printf(*(argv + 7), res);
    }
    {
        char unsigned lhs = 247;
        unsigned int rhs = 2998210528;
        unsigned int res = lhs / rhs;
        // [[print: 0]]
        printf(*(argv + 7), res);
    }

    // u8 % u32 => u32
    {
        char unsigned lhs = 110;
        unsigned int rhs = 1840167084;
        unsigned int res = lhs % rhs;
        // [[print: 110]]
        printf(*(argv + 7), res);
    }
    {
        char unsigned lhs = 199;
        unsigned int rhs = 2800079115;
        unsigned int res = lhs % rhs;
        // [[print: 199]]
        printf(*(argv + 7), res);
    }
    {
        char unsigned lhs = 192;
        unsigned int rhs = 2863657021;
        unsigned int res = lhs % rhs;
        // [[print: 192]]
        printf(*(argv + 7), res);
    }
    {
        char unsigned lhs = 36;
        unsigned int rhs = 109876255;
        unsigned int res = lhs % rhs;
        // [[print: 36]]
        printf(*(argv + 7), res);
    }
    {
        char unsigned lhs = 106;
        unsigned int rhs = 100414635;
        unsigned int res = lhs % rhs;
        // [[print: 106]]
        printf(*(argv + 7), res);
    }
    {
        char unsigned lhs = 71;
        unsigned int rhs = 3842679516;
        unsigned int res = lhs % rhs;
        // [[print: 71]]
        printf(*(argv + 7), res);
    }
    {
        char unsigned lhs = 77;
        unsigned int rhs = 1135558079;
        unsigned int res = lhs % rhs;
        // [[print: 77]]
        printf(*(argv + 7), res);
    }
    {
        char unsigned lhs = 136;
        unsigned int rhs = 1104401059;
        unsigned int res = lhs % rhs;
        // [[print: 136]]
        printf(*(argv + 7), res);
    }
    {
        char unsigned lhs = 87;
        unsigned int rhs = 725243276;
        unsigned int res = lhs % rhs;
        // [[print: 87]]
        printf(*(argv + 7), res);
    }
    {
        char unsigned lhs = 163;
        unsigned int rhs = 99060050;
        unsigned int res = lhs % rhs;
        // [[print: 163]]
        printf(*(argv + 7), res);
    }

    // u32 * u8 => u32
    {
        unsigned int lhs = 3821878247;
        char unsigned rhs = 103;
        unsigned int res = lhs * rhs;
        // [[print: 2811435505]]
        printf(*(argv + 7), res);
    }
    {
        unsigned int lhs = 3129907773;
        char unsigned rhs = 22;
        unsigned int res = lhs * rhs;
        // [[print: 138494270]]
        printf(*(argv + 7), res);
    }
    {
        unsigned int lhs = 1140330580;
        char unsigned rhs = 139;
        unsigned int res = lhs * rhs;
        // [[print: 3887127964]]
        printf(*(argv + 7), res);
    }
    {
        unsigned int lhs = 457870325;
        char unsigned rhs = 83;
        unsigned int res = lhs * rhs;
        // [[print: 3643498607]]
        printf(*(argv + 7), res);
    }
    {
        unsigned int lhs = 4261681089;
        char unsigned rhs = 142;
        unsigned int res = lhs * rhs;
        // [[print: 3863293198]]
        printf(*(argv + 7), res);
    }
    {
        unsigned int lhs = 3950196219;
        char unsigned rhs = 56;
        unsigned int res = lhs * rhs;
        // [[print: 2167656168]]
        printf(*(argv + 7), res);
    }
    {
        unsigned int lhs = 2950160282;
        char unsigned rhs = 132;
        unsigned int res = lhs * rhs;
        // [[print: 2874100584]]
        printf(*(argv + 7), res);
    }
    {
        unsigned int lhs = 3481837387;
        char unsigned rhs = 190;
        unsigned int res = lhs * rhs;
        // [[print: 124139946]]
        printf(*(argv + 7), res);
    }
    {
        unsigned int lhs = 2632772191;
        char unsigned rhs = 227;
        unsigned int res = lhs * rhs;
        // [[print: 638833213]]
        printf(*(argv + 7), res);
    }
    {
        unsigned int lhs = 3596525884;
        char unsigned rhs = 160;
        unsigned int res = lhs * rhs;
        // [[print: 4213491072]]
        printf(*(argv + 7), res);
    }

    // u32 / u8 => u32
    {
        unsigned int lhs = 1432529869;
        char unsigned rhs = 201;
        unsigned int res = lhs / rhs;
        // [[print: 7127014]]
        printf(*(argv + 7), res);
    }
    {
        unsigned int lhs = 2926028971;
        char unsigned rhs = 47;
        unsigned int res = lhs / rhs;
        // [[print: 62255935]]
        printf(*(argv + 7), res);
    }
    {
        unsigned int lhs = 764668370;
        char unsigned rhs = 235;
        unsigned int res = lhs / rhs;
        // [[print: 3253907]]
        printf(*(argv + 7), res);
    }
    {
        unsigned int lhs = 2047870516;
        char unsigned rhs = 205;
        unsigned int res = lhs / rhs;
        // [[print: 9989612]]
        printf(*(argv + 7), res);
    }
    {
        unsigned int lhs = 986986816;
        char unsigned rhs = 74;
        unsigned int res = lhs / rhs;
        // [[print: 13337659]]
        printf(*(argv + 7), res);
    }
    {
        unsigned int lhs = 3645566492;
        char unsigned rhs = 195;
        unsigned int res = lhs / rhs;
        // [[print: 18695212]]
        printf(*(argv + 7), res);
    }
    {
        unsigned int lhs = 3612188356;
        char unsigned rhs = 162;
        unsigned int res = lhs / rhs;
        // [[print: 22297458]]
        printf(*(argv + 7), res);
    }
    {
        unsigned int lhs = 2400582198;
        char unsigned rhs = 223;
        unsigned int res = lhs / rhs;
        // [[print: 10764942]]
        printf(*(argv + 7), res);
    }
    {
        unsigned int lhs = 1576840474;
        char unsigned rhs = 248;
        unsigned int res = lhs / rhs;
        // [[print: 6358227]]
        printf(*(argv + 7), res);
    }
    {
        unsigned int lhs = 1044594415;
        char unsigned rhs = 154;
        unsigned int res = lhs / rhs;
        // [[print: 6783080]]
        printf(*(argv + 7), res);
    }

    // u32 % u8 => u32
    {
        unsigned int lhs = 2142168554;
        char unsigned rhs = 253;
        unsigned int res = lhs % rhs;
        // [[print: 97]]
        printf(*(argv + 7), res);
    }
    {
        unsigned int lhs = 1250014288;
        char unsigned rhs = 30;
        unsigned int res = lhs % rhs;
        // [[print: 28]]
        printf(*(argv + 7), res);
    }
    {
        unsigned int lhs = 3234742362;
        char unsigned rhs = 225;
        unsigned int res = lhs % rhs;
        // [[print: 162]]
        printf(*(argv + 7), res);
    }
    {
        unsigned int lhs = 701309745;
        char unsigned rhs = 162;
        unsigned int res = lhs % rhs;
        // [[print: 81]]
        printf(*(argv + 7), res);
    }
    {
        unsigned int lhs = 4070485465;
        char unsigned rhs = 83;
        unsigned int res = lhs % rhs;
        // [[print: 46]]
        printf(*(argv + 7), res);
    }
    {
        unsigned int lhs = 182410960;
        char unsigned rhs = 234;
        unsigned int res = lhs % rhs;
        // [[print: 4]]
        printf(*(argv + 7), res);
    }
    {
        unsigned int lhs = 212920129;
        char unsigned rhs = 248;
        unsigned int res = lhs % rhs;
        // [[print: 225]]
        printf(*(argv + 7), res);
    }
    {
        unsigned int lhs = 1444163668;
        char unsigned rhs = 198;
        unsigned int res = lhs % rhs;
        // [[print: 178]]
        printf(*(argv + 7), res);
    }
    {
        unsigned int lhs = 1751208024;
        char unsigned rhs = 246;
        unsigned int res = lhs % rhs;
        // [[print: 198]]
        printf(*(argv + 7), res);
    }
    {
        unsigned int lhs = 1143288797;
        char unsigned rhs = 231;
        unsigned int res = lhs % rhs;
        // [[print: 35]]
        printf(*(argv + 7), res);
    }

    // i32 * i32 => i32
    {
        int lhs = -2015750594;
        int rhs = -430906186;
        int res = lhs * rhs;
        // [[print: 2017134612]]
        printf(*(argv + 3), res);
    }
    {
        int lhs = -532421455;
        int rhs = 541355389;
        int res = lhs * rhs;
        // [[print: 1217005421]]
        printf(*(argv + 3), res);
    }
    {
        int lhs = -1375576302;
        int rhs = -1351114224;
        int res = lhs * rhs;
        // [[print: -542288608]]
        printf(*(argv + 3), res);
    }
    {
        int lhs = -1102735862;
        int rhs = -1413057312;
        int res = lhs * rhs;
        // [[print: -1097084736]]
        printf(*(argv + 3), res);
    }
    {
        int lhs = 540812634;
        int rhs = 1291228103;
        int res = lhs * rhs;
        // [[print: 883930870]]
        printf(*(argv + 3), res);
    }
    {
        int lhs = 263928669;
        int rhs = -927331211;
        int res = lhs * rhs;
        // [[print: -1058243199]]
        printf(*(argv + 3), res);
    }
    {
        int lhs = -1288687871;
        int rhs = 1671089401;
        int res = lhs * rhs;
        // [[print: 1491895289]]
        printf(*(argv + 3), res);
    }
    {
        int lhs = -1558867448;
        int rhs = 842668270;
        int res = lhs * rhs;
        // [[print: 1234076528]]
        printf(*(argv + 3), res);
    }
    {
        int lhs = 247313622;
        int rhs = -955261236;
        int res = lhs * rhs;
        // [[print: 881307272]]
        printf(*(argv + 3), res);
    }
    {
        int lhs = 552302116;
        int rhs = 533292454;
        int res = lhs * rhs;
        // [[print: 1552863064]]
        printf(*(argv + 3), res);
    }

    // i32 / i32 => i32
    {
        int lhs = 1146106468;
        int rhs = -250231275;
        int res = lhs / rhs;
        // [[print: -4]]
        printf(*(argv + 3), res);
    }
    {
        int lhs = 187376103;
        int rhs = 155181467;
        int res = lhs / rhs;
        // [[print: 1]]
        printf(*(argv + 3), res);
    }
    {
        int lhs = 896126206;
        int rhs = -237357416;
        int res = lhs / rhs;
        // [[print: -3]]
        printf(*(argv + 3), res);
    }
    {
        int lhs = -647136556;
        int rhs = 1205640257;
        int res = lhs / rhs;
        // [[print: 0]]
        printf(*(argv + 3), res);
    }
    {
        int lhs = -657404533;
        int rhs = 110556297;
        int res = lhs / rhs;
        // [[print: -5]]
        printf(*(argv + 3), res);
    }
    {
        int lhs = 405434342;
        int rhs = 1308249221;
        int res = lhs / rhs;
        // [[print: 0]]
        printf(*(argv + 3), res);
    }
    {
        int lhs = -172080891;
        int rhs = 1999684074;
        int res = lhs / rhs;
        // [[print: 0]]
        printf(*(argv + 3), res);
    }
    {
        int lhs = -1062093219;
        int rhs = -35025876;
        int res = lhs / rhs;
        // [[print: 30]]
        printf(*(argv + 3), res);
    }
    {
        int lhs = -1147643045;
        int rhs = 1952154378;
        int res = lhs / rhs;
        // [[print: 0]]
        printf(*(argv + 3), res);
    }
    {
        int lhs = 832295358;
        int rhs = -1007747797;
        int res = lhs / rhs;
        // [[print: 0]]
        printf(*(argv + 3), res);
    }

    // i32 % i32 => i32
    {
        int lhs = 766192899;
        int rhs = -1227615481;
        int res = lhs % rhs;
        // [[print: 766192899]]
        printf(*(argv + 3), res);
    }
    {
        int lhs = 1721459721;
        int rhs = -1448818021;
        int res = lhs % rhs;
        // [[print: 272641700]]
        printf(*(argv + 3), res);
    }
    {
        int lhs = -481354795;
        int rhs = -1922161984;
        int res = lhs % rhs;
        // [[print: -481354795]]
        printf(*(argv + 3), res);
    }
    {
        int lhs = -1338778267;
        int rhs = 1423163830;
        int res = lhs % rhs;
        // [[print: -1338778267]]
        printf(*(argv + 3), res);
    }
    {
        int lhs = -1939618890;
        int rhs = 331074794;
        int res = lhs % rhs;
        // [[print: -284244920]]
        printf(*(argv + 3), res);
    }
    {
        int lhs = -465205881;
        int rhs = 1912009880;
        int res = lhs % rhs;
        // [[print: -465205881]]
        printf(*(argv + 3), res);
    }
    {
        int lhs = 152470989;
        int rhs = 89137728;
        int res = lhs % rhs;
        // [[print: 63333261]]
        printf(*(argv + 3), res);
    }
    {
        int lhs = -165181365;
        int rhs = -646476654;
        int res = lhs % rhs;
        // [[print: -165181365]]
        printf(*(argv + 3), res);
    }
    {
        int lhs = -1603887395;
        int rhs = -292853647;
        int res = lhs % rhs;
        // [[print: -139619160]]
        printf(*(argv + 3), res);
    }
    {
        int lhs = -785488071;
        int rhs = -1455144215;
        int res = lhs % rhs;
        // [[print: -785488071]]
        printf(*(argv + 3), res);
    }

    // u32 * u32 => u32
    {
        unsigned int lhs = 3470280314;
        unsigned int rhs = 195359260;
        unsigned int res = lhs * rhs;
        // [[print: 1986988376]]
        printf(*(argv + 7), res);
    }
    {
        unsigned int lhs = 1341893414;
        unsigned int rhs = 1633549130;
        unsigned int res = lhs * rhs;
        // [[print: 1918614268]]
        printf(*(argv + 7), res);
    }
    {
        unsigned int lhs = 2072520114;
        unsigned int rhs = 178812277;
        unsigned int res = lhs * rhs;
        // [[print: 1598264410]]
        printf(*(argv + 7), res);
    }
    {
        unsigned int lhs = 1238220340;
        unsigned int rhs = 1367327492;
        unsigned int res = lhs * rhs;
        // [[print: 2934215888]]
        printf(*(argv + 7), res);
    }
    {
        unsigned int lhs = 1880450380;
        unsigned int rhs = 3214827302;
        unsigned int res = lhs * rhs;
        // [[print: 3433839944]]
        printf(*(argv + 7), res);
    }
    {
        unsigned int lhs = 4241199250;
        unsigned int rhs = 3974509551;
        unsigned int res = lhs * rhs;
        // [[print: 184880718]]
        printf(*(argv + 7), res);
    }
    {
        unsigned int lhs = 4082807693;
        unsigned int rhs = 4126906806;
        unsigned int res = lhs * rhs;
        // [[print: 873065278]]
        printf(*(argv + 7), res);
    }
    {
        unsigned int lhs = 27040486;
        unsigned int rhs = 915042028;
        unsigned int res = lhs * rhs;
        // [[print: 3404308488]]
        printf(*(argv + 7), res);
    }
    {
        unsigned int lhs = 3198221414;
        unsigned int rhs = 573202128;
        unsigned int res = lhs * rhs;
        // [[print: 4097214176]]
        printf(*(argv + 7), res);
    }
    {
        unsigned int lhs = 2888469990;
        unsigned int rhs = 2012195590;
        unsigned int res = lhs * rhs;
        // [[print: 3649172836]]
        printf(*(argv + 7), res);
    }

    // u32 / u32 => u32
    {
        unsigned int lhs = 2072282458;
        unsigned int rhs = 1441452130;
        unsigned int res = lhs / rhs;
        // [[print: 1]]
        printf(*(argv + 7), res);
    }
    {
        unsigned int lhs = 2200029653;
        unsigned int rhs = 3148300494;
        unsigned int res = lhs / rhs;
        // [[print: 0]]
        printf(*(argv + 7), res);
    }
    {
        unsigned int lhs = 4031479772;
        unsigned int rhs = 981663985;
        unsigned int res = lhs / rhs;
        // [[print: 4]]
        printf(*(argv + 7), res);
    }
    {
        unsigned int lhs = 3409953383;
        unsigned int rhs = 1864749459;
        unsigned int res = lhs / rhs;
        // [[print: 1]]
        printf(*(argv + 7), res);
    }
    {
        unsigned int lhs = 1003848748;
        unsigned int rhs = 2779959558;
        unsigned int res = lhs / rhs;
        // [[print: 0]]
        printf(*(argv + 7), res);
    }
    {
        unsigned int lhs = 1056775542;
        unsigned int rhs = 2059174382;
        unsigned int res = lhs / rhs;
        // [[print: 0]]
        printf(*(argv + 7), res);
    }
    {
        unsigned int lhs = 2858273880;
        unsigned int rhs = 3346649347;
        unsigned int res = lhs / rhs;
        // [[print: 0]]
        printf(*(argv + 7), res);
    }
    {
        unsigned int lhs = 2199716413;
        unsigned int rhs = 2949603016;
        unsigned int res = lhs / rhs;
        // [[print: 0]]
        printf(*(argv + 7), res);
    }
    {
        unsigned int lhs = 810165642;
        unsigned int rhs = 241323840;
        unsigned int res = lhs / rhs;
        // [[print: 3]]
        printf(*(argv + 7), res);
    }
    {
        unsigned int lhs = 3303349263;
        unsigned int rhs = 2707269606;
        unsigned int res = lhs / rhs;
        // [[print: 1]]
        printf(*(argv + 7), res);
    }

    // u32 % u32 => u32
    {
        unsigned int lhs = 1146446010;
        unsigned int rhs = 3689339677;
        unsigned int res = lhs % rhs;
        // [[print: 1146446010]]
        printf(*(argv + 7), res);
    }
    {
        unsigned int lhs = 673552128;
        unsigned int rhs = 2666725966;
        unsigned int res = lhs % rhs;
        // [[print: 673552128]]
        printf(*(argv + 7), res);
    }
    {
        unsigned int lhs = 1631912914;
        unsigned int rhs = 1970178955;
        unsigned int res = lhs % rhs;
        // [[print: 1631912914]]
        printf(*(argv + 7), res);
    }
    {
        unsigned int lhs = 3677461164;
        unsigned int rhs = 1235300700;
        unsigned int res = lhs % rhs;
        // [[print: 1206859764]]
        printf(*(argv + 7), res);
    }
    {
        unsigned int lhs = 3678341959;
        unsigned int rhs = 474172301;
        unsigned int res = lhs % rhs;
        // [[print: 359135852]]
        printf(*(argv + 7), res);
    }
    {
        unsigned int lhs = 3471160058;
        unsigned int rhs = 2574189654;
        unsigned int res = lhs % rhs;
        // [[print: 896970404]]
        printf(*(argv + 7), res);
    }
    {
        unsigned int lhs = 2451701930;
        unsigned int rhs = 2410568531;
        unsigned int res = lhs % rhs;
        // [[print: 41133399]]
        printf(*(argv + 7), res);
    }
    {
        unsigned int lhs = 3158817002;
        unsigned int rhs = 1878300766;
        unsigned int res = lhs % rhs;
        // [[print: 1280516236]]
        printf(*(argv + 7), res);
    }
    {
        unsigned int lhs = 1190413219;
        unsigned int rhs = 1090018827;
        unsigned int res = lhs % rhs;
        // [[print: 100394392]]
        printf(*(argv + 7), res);
    }
    {
        unsigned int lhs = 1378928665;
        unsigned int rhs = 505183221;
        unsigned int res = lhs % rhs;
        // [[print: 368562223]]
        printf(*(argv + 7), res);
    }

    // i64 * i64 => i64
    {
        long lhs = 8693895650676104854l;
        long rhs = -9210448806406086459l;
        long res = lhs * rhs;
        // [[print: 3949525112220879214]]
        printf(*(argv + 4), res);
    }
    {
        long lhs = 3852218072788319756l;
        long rhs = 6038785365373106752l;
        long res = lhs * rhs;
        // [[print: -3887303906961769728]]
        printf(*(argv + 4), res);
    }
    {
        long lhs = 4329326210619886551l;
        long rhs = -751694812474308796l;
        long res = lhs * rhs;
        // [[print: 3917816509652272668]]
        printf(*(argv + 4), res);
    }
    {
        long lhs = 3367641107910633022l;
        long rhs = 7005942295561524850l;
        long res = lhs * rhs;
        // [[print: 7677327215696665500]]
        printf(*(argv + 4), res);
    }
    {
        long lhs = 2369409124965624724l;
        long rhs = 5168674689620045011l;
        long res = lhs * rhs;
        // [[print: -1125381486643029252]]
        printf(*(argv + 4), res);
    }
    {
        long lhs = -5960877397176118903l;
        long rhs = 1868901794435888396l;
        long res = lhs * rhs;
        // [[print: 8823594532926582636]]
        printf(*(argv + 4), res);
    }
    {
        long lhs = -9083571232433765473l;
        long rhs = 4275778136828602513l;
        long res = lhs * rhs;
        // [[print: -840256979165108977]]
        printf(*(argv + 4), res);
    }
    {
        long lhs = 5230325721733772920l;
        long rhs = 3572312526048500035l;
        long res = lhs * rhs;
        // [[print: -15618267725792920]]
        printf(*(argv + 4), res);
    }
    {
        long lhs = 1938366336964283071l;
        long rhs = -1327723995940152966l;
        long res = lhs * rhs;
        // [[print: -6540765844994275834]]
        printf(*(argv + 4), res);
    }
    {
        long lhs = 8422974732487425674l;
        long rhs = 8977890074141384144l;
        long res = lhs * rhs;
        // [[print: -9000462151440535008]]
        printf(*(argv + 4), res);
    }

    // i64 / i64 => i64
    {
        long lhs = -4356553818966558372l;
        long rhs = 6107844752273128996l;
        long res = lhs / rhs;
        // [[print: 0]]
        printf(*(argv + 4), res);
    }
    {
        long lhs = 343468043862241649l;
        long rhs = -1127306164008471050l;
        long res = lhs / rhs;
        // [[print: 0]]
        printf(*(argv + 4), res);
    }
    {
        long lhs = -8169972164321832590l;
        long rhs = 4138267040784965673l;
        long res = lhs / rhs;
        // [[print: -1]]
        printf(*(argv + 4), res);
    }
    {
        long lhs = 8929576787703721770l;
        long rhs = -6808180949780772281l;
        long res = lhs / rhs;
        // [[print: -1]]
        printf(*(argv + 4), res);
    }
    {
        long lhs = 3725463129829649846l;
        long rhs = 7234139953535968638l;
        long res = lhs / rhs;
        // [[print: 0]]
        printf(*(argv + 4), res);
    }
    {
        long lhs = 5211038055324802581l;
        long rhs = -4514422406008176802l;
        long res = lhs / rhs;
        // [[print: -1]]
        printf(*(argv + 4), res);
    }
    {
        long lhs = -4515107208570639306l;
        long rhs = 8799947802399710525l;
        long res = lhs / rhs;
        // [[print: 0]]
        printf(*(argv + 4), res);
    }
    {
        long lhs = 1774503400711083447l;
        long rhs = -1705246229572899660l;
        long res = lhs / rhs;
        // [[print: -1]]
        printf(*(argv + 4), res);
    }
    {
        long lhs = -1325818399425167488l;
        long rhs = 8644853956170686944l;
        long res = lhs / rhs;
        // [[print: 0]]
        printf(*(argv + 4), res);
    }
    {
        long lhs = 2404949585935789943l;
        long rhs = 4462061189636511185l;
        long res = lhs / rhs;
        // [[print: 0]]
        printf(*(argv + 4), res);
    }

    // i64 % i64 => i64
    {
        long lhs = 369678111416920381l;
        long rhs = 3145891578630653634l;
        long res = lhs % rhs;
        // [[print: 369678111416920381]]
        printf(*(argv + 4), res);
    }
    {
        long lhs = 5276714759242191168l;
        long rhs = 467820817131781224l;
        long res = lhs % rhs;
        // [[print: 130685770792597704]]
        printf(*(argv + 4), res);
    }
    {
        long lhs = -5043244574111327292l;
        long rhs = -4153899801548086248l;
        long res = lhs % rhs;
        // [[print: -889344772563241044]]
        printf(*(argv + 4), res);
    }
    {
        long lhs = 4338111674103894516l;
        long rhs = -1940415753101670911l;
        long res = lhs % rhs;
        // [[print: 457280167900552694]]
        printf(*(argv + 4), res);
    }
    {
        long lhs = 7622249138978959582l;
        long rhs = -856375757889220625l;
        long res = lhs % rhs;
        // [[print: 771243075865194582]]
        printf(*(argv + 4), res);
    }
    {
        long lhs = 3112496970703051706l;
        long rhs = 5483895243792502155l;
        long res = lhs % rhs;
        // [[print: 3112496970703051706]]
        printf(*(argv + 4), res);
    }
    {
        long lhs = -8939109388963344526l;
        long rhs = -5327103787383171124l;
        long res = lhs % rhs;
        // [[print: -3612005601580173402]]
        printf(*(argv + 4), res);
    }
    {
        long lhs = -6606992652424070216l;
        long rhs = -5929024355323790657l;
        long res = lhs % rhs;
        // [[print: -677968297100279559]]
        printf(*(argv + 4), res);
    }
    {
        long lhs = -6543952435032898330l;
        long rhs = -7702235069244720889l;
        long res = lhs % rhs;
        // [[print: -6543952435032898330]]
        printf(*(argv + 4), res);
    }
    {
        long lhs = 159499294734786970l;
        long rhs = 7899785618031991532l;
        long res = lhs % rhs;
        // [[print: 159499294734786970]]
        printf(*(argv + 4), res);
    }

    // u64 * u64 => u64
    {
        unsigned long lhs = 17137002774199399196ul;
        unsigned long rhs = 125008252145229122ul;
        unsigned long res = lhs * rhs;
        // [[print: 4930327417582770488]]
        printf(*(argv + 8), res);
    }
    {
        unsigned long lhs = 2930859240439992633ul;
        unsigned long rhs = 7901344412723400650ul;
        unsigned long res = lhs * rhs;
        // [[print: 4337746333023776250]]
        printf(*(argv + 8), res);
    }
    {
        unsigned long lhs = 4131379000418104304ul;
        unsigned long rhs = 4517164791312827831ul;
        unsigned long res = lhs * rhs;
        // [[print: 4373493585374941328]]
        printf(*(argv + 8), res);
    }
    {
        unsigned long lhs = 1295692225296656010ul;
        unsigned long rhs = 14325054760287086452ul;
        unsigned long res = lhs * rhs;
        // [[print: 18441862886769523848]]
        printf(*(argv + 8), res);
    }
    {
        unsigned long lhs = 4199277357104694601ul;
        unsigned long rhs = 10475931042792673375ul;
        unsigned long res = lhs * rhs;
        // [[print: 2349873931930994199]]
        printf(*(argv + 8), res);
    }
    {
        unsigned long lhs = 10054778563222469122ul;
        unsigned long rhs = 17020619688831797423ul;
        unsigned long res = lhs * rhs;
        // [[print: 4472887803408742238]]
        printf(*(argv + 8), res);
    }
    {
        unsigned long lhs = 6952194905378593346ul;
        unsigned long rhs = 5898834864672382833ul;
        unsigned long res = lhs * rhs;
        // [[print: 3430399577075848482]]
        printf(*(argv + 8), res);
    }
    {
        unsigned long lhs = 3279326738609710518ul;
        unsigned long rhs = 6733919162239856902ul;
        unsigned long res = lhs * rhs;
        // [[print: 11272453579685017668]]
        printf(*(argv + 8), res);
    }
    {
        unsigned long lhs = 9276747504007293876ul;
        unsigned long rhs = 11187299012035384371ul;
        unsigned long res = lhs * rhs;
        // [[print: 13338305952161875164]]
        printf(*(argv + 8), res);
    }
    {
        unsigned long lhs = 12068432416426131522ul;
        unsigned long rhs = 10008288182111345064ul;
        unsigned long res = lhs * rhs;
        // [[print: 1541678277040250192]]
        printf(*(argv + 8), res);
    }

    // u64 / u64 => u64
    {
        unsigned long lhs = 4085249150986604270ul;
        unsigned long rhs = 5046802737401643931ul;
        unsigned long res = lhs / rhs;
        // [[print: 0]]
        printf(*(argv + 8), res);
    }
    {
        unsigned long lhs = 14975849710219743501ul;
        unsigned long rhs = 335506038548129591ul;
        unsigned long res = lhs / rhs;
        // [[print: 44]]
        printf(*(argv + 8), res);
    }
    {
        unsigned long lhs = 6536295906544257131ul;
        unsigned long rhs = 4633968658702193930ul;
        unsigned long res = lhs / rhs;
        // [[print: 1]]
        printf(*(argv + 8), res);
    }
    {
        unsigned long lhs = 10468455395548239286ul;
        unsigned long rhs = 11551167554854845946ul;
        unsigned long res = lhs / rhs;
        // [[print: 0]]
        printf(*(argv + 8), res);
    }
    {
        unsigned long lhs = 13724698637101063064ul;
        unsigned long rhs = 10461684568846711708ul;
        unsigned long res = lhs / rhs;
        // [[print: 1]]
        printf(*(argv + 8), res);
    }
    {
        unsigned long lhs = 12281631553606451301ul;
        unsigned long rhs = 2136986742799870450ul;
        unsigned long res = lhs / rhs;
        // [[print: 5]]
        printf(*(argv + 8), res);
    }
    {
        unsigned long lhs = 5346559006955280627ul;
        unsigned long rhs = 7956597436367228727ul;
        unsigned long res = lhs / rhs;
        // [[print: 0]]
        printf(*(argv + 8), res);
    }
    {
        unsigned long lhs = 7994589845122601817ul;
        unsigned long rhs = 7104090023861257947ul;
        unsigned long res = lhs / rhs;
        // [[print: 1]]
        printf(*(argv + 8), res);
    }
    {
        unsigned long lhs = 4748232909278171686ul;
        unsigned long rhs = 5708643199990920057ul;
        unsigned long res = lhs / rhs;
        // [[print: 0]]
        printf(*(argv + 8), res);
    }
    {
        unsigned long lhs = 12741326960292955520ul;
        unsigned long rhs = 4827992696387970008ul;
        unsigned long res = lhs / rhs;
        // [[print: 2]]
        printf(*(argv + 8), res);
    }

    // u64 % u64 => u64
    {
        unsigned long lhs = 7514328652579307660ul;
        unsigned long rhs = 8274970788685243477ul;
        unsigned long res = lhs % rhs;
        // [[print: 7514328652579307660]]
        printf(*(argv + 8), res);
    }
    {
        unsigned long lhs = 7848697727370129266ul;
        unsigned long rhs = 7544491022090836365ul;
        unsigned long res = lhs % rhs;
        // [[print: 304206705279292901]]
        printf(*(argv + 8), res);
    }
    {
        unsigned long lhs = 11748636109022179036ul;
        unsigned long rhs = 8820520316607311112ul;
        unsigned long res = lhs % rhs;
        // [[print: 2928115792414867924]]
        printf(*(argv + 8), res);
    }
    {
        unsigned long lhs = 14547762828940502017ul;
        unsigned long rhs = 1137842701411732047ul;
        unsigned long res = lhs % rhs;
        // [[print: 893650411999717453]]
        printf(*(argv + 8), res);
    }
    {
        unsigned long lhs = 6237645773677191117ul;
        unsigned long rhs = 13035425409304936462ul;
        unsigned long res = lhs % rhs;
        // [[print: 6237645773677191117]]
        printf(*(argv + 8), res);
    }
    {
        unsigned long lhs = 4487609682211858618ul;
        unsigned long rhs = 12534844070979965872ul;
        unsigned long res = lhs % rhs;
        // [[print: 4487609682211858618]]
        printf(*(argv + 8), res);
    }
    {
        unsigned long lhs = 1070685649123136925ul;
        unsigned long rhs = 16800458940634209661ul;
        unsigned long res = lhs % rhs;
        // [[print: 1070685649123136925]]
        printf(*(argv + 8), res);
    }
    {
        unsigned long lhs = 16580031721005081947ul;
        unsigned long rhs = 5021531047459019183ul;
        unsigned long res = lhs % rhs;
        // [[print: 1515438578628024398]]
        printf(*(argv + 8), res);
    }
    {
        unsigned long lhs = 8754119968219087390ul;
        unsigned long rhs = 13983701473647029653ul;
        unsigned long res = lhs % rhs;
        // [[print: 8754119968219087390]]
        printf(*(argv + 8), res);
    }
    {
        unsigned long lhs = 9858738859602000821ul;
        unsigned long rhs = 9417950180237282937ul;
        unsigned long res = lhs % rhs;
        // [[print: 440788679364717884]]
        printf(*(argv + 8), res);
    }
}
