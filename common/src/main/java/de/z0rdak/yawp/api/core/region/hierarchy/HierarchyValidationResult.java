package de.z0rdak.yawp.api.core.region.hierarchy;

/**
 * Result of a hierarchy validation operation.
 *
 * Represents whether a parent-child relationship between regions
 * is valid, and if not, why it failed.
 */
public final class HierarchyValidationResult {

    private final boolean valid;
    private final FailureReason reason;

    private HierarchyValidationResult(boolean valid, FailureReason reason) {
        this.valid = valid;
        this.reason = reason;
    }

    /**
     * True if the validation passed and the parent assignment is allowed.
     */
    public boolean valid() {
        return valid;
    }

    /**
     * Reason for failure if validation did not pass.
     * If valid == true, this will be NONE.
     */
    public FailureReason reason() {
        return reason;
    }

    public static HierarchyValidationResult validResult() {
        return new HierarchyValidationResult(true, FailureReason.NONE);
    }

    public static HierarchyValidationResult invalidParentType() {
        return new HierarchyValidationResult(false, FailureReason.INVALID_PARENT_TYPE);
    }

    public static HierarchyValidationResult cycleDetected() {
        return new HierarchyValidationResult(false, FailureReason.CYCLE_DETECTED);
    }

    public static HierarchyValidationResult differentDimension() {
        return new HierarchyValidationResult(false, FailureReason.DIFFERENT_DIMENSION);
    }

    public static HierarchyValidationResult containmentFailed() {
        return new HierarchyValidationResult(false, FailureReason.CONTAINMENT_FAILED);
    }

    public static HierarchyValidationResult invalidPriority() {
        return new HierarchyValidationResult(false, FailureReason.INVALID_PRIORITY);
    }
}