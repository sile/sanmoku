package net.reduls.sanmoku.dic;

import java.io.DataInputStream;
import net.reduls.sanmoku.util.Misc;

public final class Morpheme {

    public static final int[] idMorphmesMap;
    public static final short[] morphemes;

    static {
        long beg_t = java.lang.System.currentTimeMillis();
        System.out.println("#START-3");

        {
            DataInputStream in = Misc.openDictionaryDataAsDIS("id-morphemes-map.bin");
            final int idMorphmesMapSize = Misc.readInt(in);
            idMorphmesMap = new int[idMorphmesMapSize];
            
            final byte[] buf = new byte[idMorphmesMapSize];
            try {
                in.readFully(buf, 0, buf.length);
                idMorphmesMap[0] = 0;
                for(int i=1; i < buf.length; i++) 
                    idMorphmesMap[i] = buf[i-1] + idMorphmesMap[i-1];
            } catch(Exception e) {}
            Misc.close(in);
        }

        {
            DataInputStream in = Misc.openDictionaryDataAsDIS("morpheme.bin");
            final int morphemeCount = Misc.readInt(in);
            morphemes = new short[morphemeCount*2];

            final byte[] buf = new byte[morphemeCount*4];
            try {
                in.readFully(buf, 0, buf.length);
                for(int i=0; i < morphemeCount; i++) {
                    morphemes[i*2] =   (short)((buf[i*4+0]<<8) | (buf[i*4+1]&0xff)); // posId
                    morphemes[i*2+1] = (short)((buf[i*4+2]<<8) | (buf[i*4+3]&0xff)); // cost
                }
            } catch(Exception e) {}
            
            Misc.close(in);
        }
        System.out.println("#END-3: " + (java.lang.System.currentTimeMillis()-beg_t));
    }

    public static int morphemesBegin(int surfaceId) {
        return idMorphmesMap[surfaceId];
    }

    public static int morphemesEnd(int surfaceId) {
        return idMorphmesMap[surfaceId+1];
    }
}