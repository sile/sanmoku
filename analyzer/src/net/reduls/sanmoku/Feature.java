package net.reduls.sanmoku;

import net.reduls.sanmoku.util.Misc;

// => FeatureEx
public final class Feature {
    private static final byte[] info;
    private static final byte[] data;

    static {
        info = Misc.readBytesFromFile("feature.info.bin", 6);
        data = Misc.readBytesFromFile("feature.text.bin", 2);
    }

    public static String baseform(Morpheme m) {
        long info = info(m.morphemeId);
        int baseformOffset = baseformOffset(info);
        if(baseformOffset == 0x1FFFF)
            return m.surface;
        int baseformLength = baseformLength(info);
        
        return text(baseformOffset, baseformLength);
    }

    private static long info(int i) {
        return (((long)(info[i*6+0] & 0xff) << 40) | 
                ((long)(info[i*6+1] & 0xff) << 32) | 
                ((long)(info[i*6+2] & 0xff) << 24) |
                ((long)(info[i*6+3] & 0xff) << 16) |
                ((long)(info[i*6+4] & 0xff) <<  8) | 
                ((long)(info[i*6+5] & 0xff)));
    }

    private static int baseformOffset(long info) {
        return (int)(info & 0x1FFFF);
    }

    private static int baseformLength(long info) {
        return (int)((info >> 38) & 0xF);
    }

    private static String text(int start, int length) {
        try {
            System.out.println(start+": "+length);
            return new String(data, start*2, length*2, "UTF-16BE");
        } catch (java.io.UnsupportedEncodingException ex) {
            throw new RuntimeException(ex);
        }
    }
}