package net.reduls.sanmoku.dic;

import java.io.DataInputStream;
import net.reduls.sanmoku.util.Misc;

public final class Matrix {
    private final static short[][] matrix;
    
    static {
        {
            long beg_t = java.lang.System.currentTimeMillis();
            System.out.println("#START-1");

            DataInputStream in2 = Misc.openDictionaryDataAsDIS("matrix.bin");
            final int leftNum = Misc.readInt(in2);
            final int rightNum = Misc.readInt(in2);
            final byte[] buf = new byte[leftNum*rightNum*2];
            matrix = new short[leftNum][rightNum];
            try {
                in2.readFully(buf, 0, buf.length);
                for(int l=0; l < leftNum; l++) 
                    for(int r=0; r < rightNum; r++) {
                        int i = (l*leftNum + r)*2;
                        matrix[r][l] = (short)((buf[i]<<8)+(buf[i+1]&0xff));
                    }
            } catch(Exception e) {}
            Misc.close(in2);

            System.out.println("#END-1: " + (java.lang.System.currentTimeMillis()-beg_t));
        }

        /*
        long beg_t = java.lang.System.currentTimeMillis();
        System.out.println("#START");

        DataInputStream in = Misc.openDictionaryDataAsDIS("matrix.bin");

        final int leftNum = Misc.readInt(in);
        final int rightNum = Misc.readInt(in);
        matrix = new short[leftNum][rightNum];

        try {
        for(int l=0; l < leftNum; l++) 
            for(int r=0; r < rightNum; r++)
                matrix[r][l] = in.readShort(); //Misc.readShort(in);
        } catch(Exception e) {}
        System.out.println("#END: " + (java.lang.System.currentTimeMillis()-beg_t));
        Misc.close(in);
        */
    }

    public static short linkCost(short leftId, short rightId) {
        return matrix[rightId][leftId];
    }
}
