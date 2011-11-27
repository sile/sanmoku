package net.reduls.sanmoku;

public final class Morpheme {
    public final String surface;
    public final String feature;
    public final int start;
    final int morphemeId;
    
    public Morpheme(String surface, String feature, int start, int morphemeId) {
        this.surface = surface;
        this.feature = feature;
        this.start = start;
        this.morphemeId = morphemeId;
    }
}