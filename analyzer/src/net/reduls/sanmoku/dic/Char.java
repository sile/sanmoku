package net.reduls.sanmoku.dic;

import java.io.DataInputStream;
import net.reduls.sanmoku.util.Misc;

public final class Char {
    public static final class Category {
        public final int id;
        public final boolean invoke;
        public final boolean group;
        public final byte length;

        public Category(int id, boolean invoke, boolean group, byte length) {
            this.id = id;
            this.invoke = invoke;
            this.group = group;
            this.length = length;
        }
    }
    
    private final static Category[] charCategorys;
    private final static byte[] charInfos;

    static {
        DataInputStream in = Misc.openDictionaryDataAsDIS("category.bin");
        
        final int charCategoryNum = Misc.readInt(in);
        charCategorys = new Category[charCategoryNum];
        for(int i=0; i < charCategoryNum; i++)
            charCategorys[i] = new Category(i,
                                            Misc.readByte(in)==1,
                                            Misc.readByte(in)==1,
                                            Misc.readByte(in));
        Misc.close(in);
        
        charInfos = Misc.readBytesFromFile("code.bin", 6);
    }

    public static final Category category(char c) {
        return charCategorys[findNode(c)>>16];
    }

    public static final boolean isCompatible(char c1, char c2) {
        return (compatibleMask(c1) & compatibleMask(c2)) != 0;
    }

    private static final int compatibleMask(char c) {
        return findNode(c)&0xFFFF;
    }

    public static final int findNode (char c) {
        int beg = 0;
        int end = charInfos.length/6;

        for(;;) {
            final int mid = beg+(end-beg)/2;
            if(end-beg == 1) 
                return nodeValue(beg);
            else if(c < nodeCode(mid))
                end = mid;
            else if(c >= nodeCode(mid))
                beg = mid;
        }
    }
    
    public static final int nodeCode (int i) {
        return 
            (int)((charInfos[i*6+0]&0xFF)<<16) |
            (int)((charInfos[i*6+1]&0xFF)<<8) |
            (int)((charInfos[i*6+2]&0xFF)<<0);
    }

    public static final int nodeValue (int i) {
        return 
            (int)((charInfos[i*6+3]&0xFF)<<16) |
            (int)((charInfos[i*6+4]&0xFF)<<8) |
            (int)((charInfos[i*6+5]&0xFF)<<0);
    }
}
