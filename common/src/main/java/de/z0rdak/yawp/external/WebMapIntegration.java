package de.z0rdak.yawp.external;

import de.z0rdak.yawp.api.events.region.RegionEvent;

public interface WebMapIntegration {

    void on(RegionEvent.Create event);

    void on(RegionEvent.UpdateArea event);

    void on(RegionEvent.Rename event);

    void on(RegionEvent.Remove event);
}
