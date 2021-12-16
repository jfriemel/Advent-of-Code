package de.jfriemel.aoc.days;

import org.junit.Test;

import java.util.Arrays;
import java.util.List;

import static org.junit.Assert.assertEquals;

public class Day16Test {

    private static final String INPUT_STRING_1_A = "D2FE28";
    private static final String INPUT_STRING_1_B = "38006F45291200";
    private static final String INPUT_STRING_1_C = "EE00D40C823060";
    private static final String INPUT_STRING_1_D = "8A004A801A8002F478";
    private static final String INPUT_STRING_1_E = "620080001611562C8802118E34";
    private static final String INPUT_STRING_1_F = "C0015000016115A2E0802F182340";
    private static final String INPUT_STRING_1_G = "A0016C880162017C3686B18A3D4780";

    private static final String INPUT_STRING_2_A = "C200B40A82";
    private static final String INPUT_STRING_2_B = "04005AC33890";
    private static final String INPUT_STRING_2_C = "880086C3E88112";
    private static final String INPUT_STRING_2_D = "CE00C43D881120";
    private static final String INPUT_STRING_2_E = "D8005AC2A8F0";
    private static final String INPUT_STRING_2_F = "F600BC2D8F";
    private static final String INPUT_STRING_2_G = "9C005AC2F8F0";
    private static final String INPUT_STRING_2_H = "9C0141080250320F1802104A08";

    private static final List<String> INPUT_1_A = Arrays.asList(INPUT_STRING_1_A.split("\n"));
    private static final List<String> INPUT_1_B = Arrays.asList(INPUT_STRING_1_B.split("\n"));
    private static final List<String> INPUT_1_C = Arrays.asList(INPUT_STRING_1_C.split("\n"));
    private static final List<String> INPUT_1_D = Arrays.asList(INPUT_STRING_1_D.split("\n"));
    private static final List<String> INPUT_1_E = Arrays.asList(INPUT_STRING_1_E.split("\n"));
    private static final List<String> INPUT_1_F = Arrays.asList(INPUT_STRING_1_F.split("\n"));
    private static final List<String> INPUT_1_G = Arrays.asList(INPUT_STRING_1_G.split("\n"));

    private static final List<String> INPUT_2_A = Arrays.asList(INPUT_STRING_2_A.split("\n"));
    private static final List<String> INPUT_2_B = Arrays.asList(INPUT_STRING_2_B.split("\n"));
    private static final List<String> INPUT_2_C = Arrays.asList(INPUT_STRING_2_C.split("\n"));
    private static final List<String> INPUT_2_D = Arrays.asList(INPUT_STRING_2_D.split("\n"));
    private static final List<String> INPUT_2_E = Arrays.asList(INPUT_STRING_2_E.split("\n"));
    private static final List<String> INPUT_2_F = Arrays.asList(INPUT_STRING_2_F.split("\n"));
    private static final List<String> INPUT_2_G = Arrays.asList(INPUT_STRING_2_G.split("\n"));
    private static final List<String> INPUT_2_H = Arrays.asList(INPUT_STRING_2_H.split("\n"));

    @Test
    public void testPart1_a() {
        String result = new Day16().part1(INPUT_1_A);
        assertEquals("6", result);
    }

    @Test
    public void testPart1_b() {
        String result = new Day16().part1(INPUT_1_B);
        assertEquals("9", result);
    }

    @Test
    public void testPart1_c() {
        String result = new Day16().part1(INPUT_1_C);
        assertEquals("14", result);
    }

    @Test
    public void testPart1_d() {
        String result = new Day16().part1(INPUT_1_D);
        assertEquals("16", result);
    }

    @Test
    public void testPart1_e() {
        String result = new Day16().part1(INPUT_1_E);
        assertEquals("12", result);
    }

    @Test
    public void testPart1_f() {
        String result = new Day16().part1(INPUT_1_F);
        assertEquals("23", result);
    }

    @Test
    public void testPart1_g() {
        String result = new Day16().part1(INPUT_1_G);
        assertEquals("31", result);
    }

    // sum
    @Test
    public void testPart2_a() {
        String result = new Day16().part2(INPUT_2_A);
        assertEquals("3", result);
    }

    // product
    @Test
    public void testPart2_b() {
        String result = new Day16().part2(INPUT_2_B);
        assertEquals("54", result);
    }

    // minimum
    @Test
    public void testPart2_c() {
        String result = new Day16().part2(INPUT_2_C);
        assertEquals("7", result);
    }

    // maximum
    @Test
    public void testPart2_d() {
        String result = new Day16().part2(INPUT_2_D);
        assertEquals("9", result);
    }

    // less than
    @Test
    public void testPart2_e() {
        String result = new Day16().part2(INPUT_2_E);
        assertEquals("1", result);
    }

    // greater than
    @Test
    public void testPart2_f() {
        String result = new Day16().part2(INPUT_2_F);
        assertEquals("0", result);
    }

    // equal
    @Test
    public void testPart2_g() {
        String result = new Day16().part2(INPUT_2_G);
        assertEquals("0", result);
    }

    // equal, sum, product
    @Test
    public void testPart2_h() {
        String result = new Day16().part2(INPUT_2_H);
        assertEquals("1", result);
    }

}
