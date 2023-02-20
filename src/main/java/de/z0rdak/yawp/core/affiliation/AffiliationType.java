package de.z0rdak.yawp.core.affiliation;

public enum AffiliationType {
    PLAYER("player"),
    TEAM("team");

    public final String name;

    AffiliationType(String name) {
        this.name = name;
    }

    public static AffiliationType of(String name) {
        switch (name) {
            case "player":
                return PLAYER;
            case "team":
                return TEAM;

            default:
                return null;
        }
    }

    @Override
    public String toString() {
        return name;
    }
}
