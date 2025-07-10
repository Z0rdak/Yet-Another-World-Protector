package de.z0rdak.yawp.api.visualization;

import de.z0rdak.yawp.core.area.BlockDisplayProperties;
import de.z0rdak.yawp.core.area.TeleportAnchor;
import de.z0rdak.yawp.core.area.TextDisplayProperties;
import net.minecraft.world.entity.Entity;

public class TpAnchorVisualization {

    private final TeleportAnchor tpAnchor;
    private final BlockVisualization blockDisplay;
    private final TextVisualization textDisplay;

    public TpAnchorVisualization(TeleportAnchor tpAnchor, BlockDisplayProperties blockDisplayProperties, TextDisplayProperties textDisplayProperties) {
        this.tpAnchor = tpAnchor;
        this.blockDisplay = new BlockVisualization(tpAnchor.getPos(), blockDisplayProperties);
        this.textDisplay = new TextVisualization(tpAnchor.getPos(), textDisplayProperties);
    }

    public void trackTpAnchorDisplay(Entity textEntity, Entity blockEntity){
        this.blockDisplay.track(blockEntity);
        this.textDisplay.track(textEntity);
    }

    public void discardTpAnchorDisplay(){
        this.blockDisplay.discard();
        this.textDisplay.discard();
    }


}
