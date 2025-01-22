package de.z0rdak.yawp.external;

import de.z0rdak.yawp.api.events.region.RegionEvent;
import de.z0rdak.yawp.api.permission.Permissions;
import de.z0rdak.yawp.core.region.IMarkableRegion;

import java.util.stream.Collectors;

public interface WebMapIntegration {

    void on(RegionEvent.Create event);

    void on(RegionEvent.UpdateArea event);

    void on(RegionEvent.Rename event);

    void on(RegionEvent.Remove event);

    void onLoad();

    default String getDetails(IMarkableRegion region, String name) {
        var owners = region.getGroup(Permissions.OWNER);
        var ownerNames = owners.getPlayers().values().stream()
                .map("<b> %s </b>"::formatted)
                .collect(Collectors.joining(", "));
        return "%s owned by %s".formatted(name, ownerNames);
    }
}
