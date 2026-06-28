package de.z0rdak.yawp.api.core.region.hierarchy;

public enum FailureReason {
    NONE,
    INVALID_PARENT_TYPE,
    DIFFERENT_DIMENSION,
    CONTAINMENT_FAILED,
    INVALID_PRIORITY,
    CYCLE_DETECTED;
}