package de.z0rdak.yawp.handler.flags;


public class FlagState {
    public static final FlagState ALLOWED = new FlagState(true);
    public static final FlagState DENIED = new FlagState(false);
    public static final FlagState UNDEFINED = new FlagState();

    private final boolean value;
    private final boolean isDefined;

    private FlagState() {
        this.value = false;
        this.isDefined = false;
    }

    public FlagState(boolean value) {
        this.value = value;
        this.isDefined = true;
    }

    public static FlagState fromBoolean(boolean value) {
        return value ? ALLOWED : DENIED;
    }

    public boolean isDefined() {
        return isDefined;
    }

    public boolean isTrue() {
        return value;
    }

    public boolean isFalse() {
        return !value;
    }

    public FlagState and(FlagState other) {
        if (!isDefined()) {
            return other;
        }
        if (!other.isDefined()) {
            return this;
        }
        return value && other.value ? ALLOWED : DENIED;
    }

    public FlagState or(FlagState other) {
        if (!isDefined()) {
            return other;
        }
        if (!other.isDefined()) {
            return this;
        }
        return value || other.value ? ALLOWED : DENIED;
    }

    public FlagState not() {
        if (!isDefined()) {
            return this;
        }
        return value ? DENIED : ALLOWED;
    }

}
