package net.reduls.sanmoku.dic;

import java.io.DataInputStream;
import net.reduls.sanmoku.util.Misc;

public final class Matrix {
    private final static short[][] matrix;
    
    static {
        {
            long beg_t = java.lang.System.currentTimeMillis();
            System.out.println("#START-1");

            DataInputStream in = Misc.openDictionaryDataAsDIS("matrix.bin");
            final int leftNum = Misc.readInt(in);
            final int rightNum = Misc.readInt(in);
            final byte[] buf = new byte[leftNum*rightNum*2];
            matrix = new short[leftNum][rightNum];
            try {
                in.readFully(buf, 0, buf.length);
                for(int l=0; l < leftNum; l++) 
                    for(int r=0; r < rightNum; r++) {
                        final int i = (l*leftNum + r)*2;
                        matrix[r][l] = (short)((buf[i]<<8) | (buf[i+1]&0xff));
                    }
            } catch(Exception e) {}
            Misc.close(in);

            System.out.println("#END-1: " + (java.lang.System.currentTimeMillis()-beg_t));
        }
    }

    public static short linkCost(short leftId, short rightId) {
        return matrix[rightId][leftId];
    }
}
