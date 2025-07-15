package de.z0rdak.yawp.core.flag;

import java.util.Set;

public record FlagMetaInfo(FlagType type, Set<FlagTag> tags, FlagFrequency frequency) {

}
