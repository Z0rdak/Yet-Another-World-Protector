package de.z0rdak.yawp.core.flag;

import java.util.HashSet;
import java.util.Set;

import static de.z0rdak.yawp.core.flag.FlagType.LIST_FLAG;

@Deprecated
public class ListFlag extends Flag {

    public Set<String> resourceKey;

    public ListFlag(String flagIdentifier, boolean isAllowed) {
        super(flagIdentifier, LIST_FLAG, isAllowed);
        resourceKey = new HashSet<>(0);
    }

    public boolean containsKey(String key) {
        return this.resourceKey.contains(key);
    }

    public boolean allows(String key) {
        return this.containsKey(key) && doesOverride();
    }
}
