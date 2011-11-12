package net.reduls.sanmoku.dic;

import java.io.DataInputStream;
import net.reduls.sanmoku.util.Misc;

public final class Matrix {
    private final static byte[] matrix;
    private final static int leftNum;
    private final static byte[] posid_map;
    private final static byte[] val;
    
    static {
        posid_map = Misc.readBytesFromFile("posid-map.bin", 2);
        val =  Misc.readBytesFromFile("matrix.map", 2);
        {
            DataInputStream in = Misc.openDictionaryDataAsDIS("matrix.bin");
            final int count = Misc.readInt(in);
            leftNum = Misc.readInt(in);
            matrix = new byte[count*7];
            try {
                in.readFully(matrix, 0, matrix.length);
            } catch(Exception e) {
                throw new RuntimeException(e);
            }
            Misc.close(in); 
        }
    }
    
    public static short linkCost(short leftId, short rightId) {
        final int i = posid(leftId)*leftNum + posid(rightId);
        final long n = node(i / 4);
        final int mi = (int)(n >> ((i % 4)*14)) & 0x3FFF;
        return (short)((val[mi*2]<<8) | (val[mi*2+1]&0xFF));
    }

    private static short posid(short id) {
        return (short)((posid_map[id*2]<<8) | (posid_map[id*2+1]&0xFF));
    }

    private static long node(int i) {
        return (((long)(matrix[i*7+0] & 0xff) << 48) | 
                ((long)(matrix[i*7+1] & 0xff) << 40) | 
                ((long)(matrix[i*7+2] & 0xff) << 32) | 
                ((long)(matrix[i*7+3] & 0xff) << 24) |
                ((long)(matrix[i*7+4] & 0xff) << 16) |
                ((long)(matrix[i*7+5] & 0xff) <<  8) | 
                ((long)(matrix[i*7+6] & 0xff)));        
    }
}
