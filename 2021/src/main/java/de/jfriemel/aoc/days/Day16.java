package de.jfriemel.aoc.days;

import de.jfriemel.aoc.Day;

import java.util.ArrayList;
import java.util.List;

public class Day16 implements Day {

    @Override
    public String part1(List<String> input) {
        return Integer.toString(addVersions(parsePacket(hexToBin(input.get(0)))));
    }

    @Override
    public String part2(List<String> input) {
        return Long.toString(evaluatePacket(parsePacket(input.get(0))));
    }

    private int addVersions(final Packet packet) {
        int sum = packet.version;
        if (packet instanceof OperatorPacket) {
            for (final Packet nextPacket : ((OperatorPacket) packet).packetsContained) {
                sum += addVersions(nextPacket);
            }
        }
        return sum;
    }

    private long evaluatePacket(final Packet packet) {
        long value = 0L;
        if (packet instanceof LiteralPacket) {
            return ((LiteralPacket) packet).literal;
        } else if (packet instanceof OperatorPacket) {
            final List<Packet> subPackets = ((OperatorPacket) packet).packetsContained;
            switch (packet.type) {
                case 0: // sum
                    for (final Packet subPacket : subPackets) {
                        value += evaluatePacket(subPacket);
                    }
                    break;
                case 1: // product
                    value = 1L;
                    for (final Packet subPacket : subPackets) {
                        value *= evaluatePacket(subPacket);
                    }
                    break;
                case 2: // minimum
                    value = Long.MAX_VALUE;
                    for (final Packet subPacket : subPackets) {
                        final long eval = evaluatePacket(subPacket);
                        if (eval < value)
                            value = eval;
                    }
                    break;
                case 3: // maximum
                    for (final Packet subPacket : subPackets) {
                        final long eval = evaluatePacket(subPacket);
                        if (eval > value)
                            value = eval;
                    }
                    break;
                case 5: // greater than
                    value = evaluatePacket(subPackets.get(0)) > evaluatePacket(subPackets.get(1)) ? 1L : 0L;
                    break;
                case 6: // less than
                    value = evaluatePacket(subPackets.get(0)) < evaluatePacket(subPackets.get(1)) ? 1L : 0L;
                    break;
                case 7: // equal to
                    value = evaluatePacket(subPackets.get(0)) == evaluatePacket(subPackets.get(1)) ? 1L : 0L;
            }
        }
        return value;
    }

    private Packet parsePacket(String binary) {
        byte version = (byte) binToDec(binary.substring(0,3));
        byte type = (byte) binToDec(binary.substring(3,6));

        final Packet packet;
        if (type == 4) {
            packet = new LiteralPacket();
        } else {
            packet = new OperatorPacket();
        }
        packet.version = version;
        packet.type = type;

        binary = binary.substring(6);
        packet.bitsUsed += 6;

        boolean last = false;
        StringBuilder literalBuilder = new StringBuilder();
        if (packet instanceof LiteralPacket) {
            while (!last) {
                if (binary.charAt(0) == '0')
                    last = true;
                literalBuilder.append(binary, 1, 5);
                binary = binary.substring(5);
                packet.bitsUsed += 5;
            }
            ((LiteralPacket) packet).literal = binToDec(literalBuilder.toString());
        } else {
            ((OperatorPacket) packet).lengthTypeID = binary.charAt(0) == '1';
            binary = binary.substring(1);
            packet.bitsUsed++;

            if (((OperatorPacket) packet).lengthTypeID) {
                ((OperatorPacket) packet).numberOfPackets = (int) binToDec(binary.substring(0, 11));
                binary = binary.substring(11);
                packet.bitsUsed += 11;
            } else {
                ((OperatorPacket) packet).length = (int) binToDec(binary.substring(0, 15));
                binary = binary.substring(15);
                packet.bitsUsed += 15;
            }

            while (((OperatorPacket) packet).length > 0 && ((OperatorPacket) packet).numberOfPackets > 0) {
                Packet nextPacket = parsePacket(binary);
                binary = binary.substring(nextPacket.bitsUsed);
                packet.bitsUsed += (nextPacket.bitsUsed);
                ((OperatorPacket) packet).packetsContained.add(nextPacket);

                ((OperatorPacket) packet).length -= nextPacket.bitsUsed;
                ((OperatorPacket) packet).numberOfPackets--;
            }
        }

        return packet;
    }

    private String hexToBin(final String input) {
        final StringBuilder binaryBuilder = new StringBuilder();
        for (final char symbol : input.toCharArray()) {
            switch (symbol) {
                case '0' -> binaryBuilder.append("0000");
                case '1' -> binaryBuilder.append("0001");
                case '2' -> binaryBuilder.append("0010");
                case '3' -> binaryBuilder.append("0011");
                case '4' -> binaryBuilder.append("0100");
                case '5' -> binaryBuilder.append("0101");
                case '6' -> binaryBuilder.append("0110");
                case '7' -> binaryBuilder.append("0111");
                case '8' -> binaryBuilder.append("1000");
                case '9' -> binaryBuilder.append("1001");
                case 'A' -> binaryBuilder.append("1010");
                case 'B' -> binaryBuilder.append("1011");
                case 'C' -> binaryBuilder.append("1100");
                case 'D' -> binaryBuilder.append("1101");
                case 'E' -> binaryBuilder.append("1110");
                case 'F' -> binaryBuilder.append("1111");
            }
        }
        return binaryBuilder.toString();
    }

    private long binToDec(final String binary) {
        long dec = 0;
        for (final char bit : binary.toCharArray()) {
            dec = dec * 2 + (bit == '1' ? 1 : 0);
        }
        return dec;
    }

}

class Packet {
    byte version;
    byte type;
    int bitsUsed = 0;
}

class LiteralPacket extends Packet {
    long literal;
}

class OperatorPacket extends Packet {
    boolean lengthTypeID;
    int length = Integer.MAX_VALUE;
    int numberOfPackets = Integer.MAX_VALUE;
    List<Packet> packetsContained = new ArrayList<>();
}