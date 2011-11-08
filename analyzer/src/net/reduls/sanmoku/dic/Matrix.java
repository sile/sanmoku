package net.reduls.sanmoku.dic;

import java.io.DataInputStream;
import net.reduls.sanmoku.util.Misc;

public final class Matrix {
    private final static byte[] matrix;
    private final static int leftNum;
    private final static byte[] posid_map;
    
    static {
        posid_map = Misc.readBytesFromFile("posid-map.bin", 2);
        
        {
            DataInputStream in = Misc.openDictionaryDataAsDIS("matrix.bin");
            leftNum = Misc.readInt(in);
            final int rightNum = Misc.readInt(in);
            matrix = new byte[leftNum*rightNum*2];
            try {
                in.readFully(matrix, 0, matrix.length);
            } catch(Exception e) {
                throw new RuntimeException(e);
            }
            Misc.close(in); 
        }
    }
    
    public static short linkCost(short leftId, short rightId) {
        final int i = (posid(leftId)*leftNum + posid(rightId))*2;
        return (short)((matrix[i]<<8) | (matrix[i+1]&0xFF));
    }

    private static short posid(short id) {
        return (short)((posid_map[id*2]<<8) | (posid_map[id*2+1]&0xFF));
    }
}
