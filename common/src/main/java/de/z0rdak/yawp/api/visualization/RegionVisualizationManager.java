package de.z0rdak.yawp.api.visualization;

import de.z0rdak.yawp.core.area.BlockDisplayProperties;
import de.z0rdak.yawp.core.area.DisplayType;
import de.z0rdak.yawp.core.area.TeleportAnchor;
import de.z0rdak.yawp.core.area.TextDisplayProperties;
import de.z0rdak.yawp.core.region.IMarkableRegion;
import net.minecraft.core.BlockPos;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.entity.Entity;

import java.util.HashMap;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

import static de.z0rdak.yawp.api.visualization.VisualizationUtil.*;

public class RegionVisualizationManager {

    private final IMarkableRegion region;
    private final RegionVisualization hull;
    private final RegionVisualization frame;
    private final RegionVisualization minimalOutline;
    private final RegionVisualization marked;
    // TODO:
    private final Map<String, TpAnchorVisualization> tpAnchorVisualizations;

    public RegionVisualizationManager(IMarkableRegion region) {
        this.region = region;
        var blockDisplayProperties = region.getArea().getDisplay();
        this.hull = new RegionVisualization(blockDisplayProperties);
        this.frame = new RegionVisualization(blockDisplayProperties);
        this.minimalOutline = new RegionVisualization(blockDisplayProperties);
        this.marked = new RegionVisualization(blockDisplayProperties);
        this.tpAnchorVisualizations = new HashMap<>();
        // Creates entries for each anchor present in region, BUT
        // TODO: Needs to be updated when an anchor is added, removed, updated -> event
        region.getTpAnchors().getAnchors().forEach(anchor -> {
            TextDisplayProperties textDisplayProperties = new TextDisplayProperties(anchor.getName());
            TpAnchorVisualization tpAnchorVisualization = new TpAnchorVisualization(anchor, blockDisplayProperties, textDisplayProperties);
            this.tpAnchorVisualizations.put(anchor.getName(), tpAnchorVisualization);
        });
    }

    public Set<BlockPos> blocksForDisplayType(DisplayType type) {
        return switch (type) {
            case FRAME -> region.getArea().getFrame();
            case HULL -> region.getArea().getHull();
            case MINIMAL -> region.getArea().getMinimalOutline();
            case MARKED -> region.getArea().markedBlocks();
        };
    }

    public void show(DisplayType displayType, BlockDisplayProperties displayProperties, ServerLevel level) {
        Set<BlockPos> blocks = blocksForDisplayType(displayType);
        blocks.stream()
                .filter(pos -> ! switch (displayType) {
                    case FRAME -> this.frame.doesTrackEntityAt(pos);
                    case HULL -> this.hull.doesTrackEntityAt(pos);
                    case MINIMAL -> this.minimalOutline.doesTrackEntityAt(pos);
                    case MARKED -> this.marked.doesTrackEntityAt(pos);
                })
                .forEach(pos -> {
                    var maybeEntity = createBlockDisplayEntity(level, region.getName(), pos, displayProperties);
                    if (maybeEntity.isPresent()) {
                        var entity = maybeEntity.get();
                        entity.addTag(VisualizationManager.REGION_BLOCK_DISPLAY_TAG.toString());
                        switch (displayType) {
                            case FRAME -> this.frame.trackBlockDisplay(pos, entity);
                            case HULL -> this.hull.trackBlockDisplay(pos, entity);
                            case MINIMAL -> this.minimalOutline.trackBlockDisplay(pos, entity);
                            case MARKED -> this.marked.trackBlockDisplay(pos, entity);
                        }
                        level.addFreshEntity(entity);
                    }
                });
    }

    public void show(DisplayType displayType, ServerLevel level) {
        show(displayType, region.getArea().getDisplay(), level);
    }

    public void hide(DisplayType displayType) {
        switch (displayType) {
            case FRAME -> this.frame.discardEntities();
            case HULL -> this.hull.discardEntities();
            case MINIMAL -> this.minimalOutline.discardEntities();
            case MARKED -> this.marked.discardEntities();
        }
    }

    public void show(TeleportAnchor anchor, BlockDisplayProperties displayProperties, TextDisplayProperties textDisplayProperties, ServerLevel level) {
        var tpAnchorVisualization = this.tpAnchorVisualizations.get(anchor.getName());
    }

    public void hide(TeleportAnchor anchor) {
        var tpAnchorVisualization = this.tpAnchorVisualizations.get(anchor.getName());
    }

    public void updateDisplay(BlockDisplayProperties displayProperties, DisplayType displayType) {
        this.frame.updateDisplay(displayProperties, false);
        this.hull.updateDisplay(displayProperties, false);
        this.minimalOutline.updateDisplay(displayProperties, false);
        this.marked.updateDisplay(displayProperties, false);
        switch (displayType) {
            case FRAME -> this.frame.updateDisplay(displayProperties, true);
            case HULL -> this.hull.updateDisplay(displayProperties, true);
            case MINIMAL -> this.minimalOutline.updateDisplay(displayProperties, true);
            case MARKED -> this.marked.updateDisplay(displayProperties, true);
        }
    }

    public void updateDisplay(BlockDisplayProperties displayProperties, boolean refresh) {
        this.frame.updateDisplay(displayProperties, refresh);
        this.hull.updateDisplay(displayProperties, refresh);
        this.minimalOutline.updateDisplay(displayProperties, refresh);
        this.marked.updateDisplay(displayProperties, refresh);
    }
}
