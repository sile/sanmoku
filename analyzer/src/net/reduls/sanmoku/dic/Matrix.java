package net.reduls.sanmoku.dic;

import java.io.DataInputStream;
import net.reduls.sanmoku.util.Misc;

public final class Matrix {
    private final static byte[] matrix;
    private final static int leftNum;
    
    static {
        DataInputStream in = Misc.openDictionaryDataAsDIS("matrix.bin");
        leftNum = Misc.readInt(in);
        final int rightNum = Misc.readInt(in);
        matrix = new byte[leftNum*rightNum*2];
        try {
            in.readFully(matrix, 0, matrix.length);
        } catch(Exception e) {}
        Misc.close(in);
    }

    public static short linkCost(short leftId, short rightId) {
        final int i = (leftId*leftNum + rightId)*2;
        return (short)((matrix[i]<<8) | (matrix[i+1]&0xff));
    }
}
