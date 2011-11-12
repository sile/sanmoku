package net.reduls.sanmoku.util;

import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.BufferedReader;
import java.io.DataInput;
import java.io.DataInputStream;
import java.io.BufferedInputStream;
import java.io.Closeable;
import java.io.IOException;

public final class Misc {
    public static InputStream openDictionaryData(String filename) {
        return Misc.class.getResourceAsStream("/net/reduls/sanmoku/dicdata/"+filename);
    }

    public static DataInputStream openDictionaryDataAsDIS(String filename) {
        return new DataInputStream(new BufferedInputStream(openDictionaryData(filename), 80960));
    }

    public static BufferedReader openDictionaryDataAsBR(String filename) {
        try {
            return new BufferedReader(new InputStreamReader(openDictionaryData(filename),"UTF-8"), 80960);
        } catch(IOException ex) {
            throw new AssertionError(ex.getMessage());
        }
    }

    public static String readLine(BufferedReader in) {
        try {
            return in.readLine();
        } catch (IOException ex) {
            throw new AssertionError(ex.getMessage());
        }
    }
    
    public static void close(Closeable in) {
        try {
            in.close();
        } catch (IOException ex) {
            throw new AssertionError(ex.getMessage());
        }
    }

    public static long readLong(DataInput in) {
        try {
            return in.readLong();
        } catch (IOException ex) {
            throw new AssertionError(ex.getMessage());
        }
    }

    public static int readInt(DataInput in) {
        try {
            return in.readInt();
        } catch (IOException ex) {
            throw new AssertionError(ex.getMessage());
        }
    }

    public static short readShort(DataInput in) {
        try {
            return in.readShort();
        } catch (IOException ex) {
            throw new AssertionError(ex.getMessage());
        }
    }

    public static byte readByte(DataInput in) {
        try {
            return in.readByte();
        } catch (IOException ex) {
            throw new AssertionError(ex.getMessage());
        }
    }

    public static char readChar(DataInput in) {
        try {
            return in.readChar();
        } catch (IOException ex) {
            throw new AssertionError(ex.getMessage());
        }
    }

    public static byte[] readBytesFromFile(String filename, int unit) {
        DataInputStream in = Misc.openDictionaryDataAsDIS(filename);
        final int count = Misc.readInt(in);
        final byte[] buf = new byte[count*unit];
        try {
            in.readFully(buf, 0, buf.length);
        } catch(Exception e) {
            throw new RuntimeException(e);
        }
        return buf;
    }

    public static byte[] readBytesFromFile(String filename, int count, int unit) {
        DataInputStream in = Misc.openDictionaryDataAsDIS(filename);
        final byte[] buf = new byte[count*unit];
        try {
            in.readFully(buf, 0, buf.length);
        } catch(Exception e) {
            throw new RuntimeException(e);
        }
        return buf;
    }
    
    public static int readIntFromFile(String filename) {
        DataInputStream in = Misc.openDictionaryDataAsDIS(filename);
        int i = Misc.readInt(in);
        Misc.close(in);
        return i;
    }
}