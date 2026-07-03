package de.z0rdak.yawp.api.visualization;

import de.z0rdak.yawp.core.area.visuals.BlockDisplayProperties;
import de.z0rdak.yawp.core.area.anchors.TeleportAnchor;
import de.z0rdak.yawp.core.area.visuals.TextDisplayProperties;
import net.minecraft.world.entity.Entity;
import org.apache.commons.lang3.NotImplementedException;

public class TpAnchorVisualization {

    private TeleportAnchor tpAnchor;
    private BlockVisualization blockDisplay;
    private TextVisualization textDisplay;

    public TpAnchorVisualization(TeleportAnchor tpAnchor, BlockDisplayProperties blockDisplayProperties, TextDisplayProperties textDisplayProperties) {
        this.tpAnchor = tpAnchor;
        this.blockDisplay = new BlockVisualization(tpAnchor.getPos(), blockDisplayProperties);
        this.textDisplay = new TextVisualization(tpAnchor.getPos(), textDisplayProperties);
    }

    public void trackTpAnchorDisplay(Entity textEntity, Entity blockEntity){
        this.blockDisplay.track(blockEntity);
        this.textDisplay.track(textEntity);
    }

    public void trackTpAnchorBlockDisplay(Entity blockEntity){
        this.blockDisplay.track(blockEntity);
    }

    public void updateBlockPosition(TeleportAnchor anchor){
        this.tpAnchor = anchor;
        boolean exists = this.blockDisplay.doesTrack();
        if (exists) {
            this.blockDisplay.move(anchor.getPos());
        }
    }

    public void updateText(TeleportAnchor anchor){
        this.tpAnchor = anchor;
        throw new NotImplementedException("TpAnchorVisualization.updateTextPosition");
    }

    public void discardTpAnchorDisplay(){
        this.blockDisplay.discard();
        this.textDisplay.discard();
    }


}
