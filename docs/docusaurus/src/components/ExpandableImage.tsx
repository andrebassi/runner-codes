import React, { useState } from 'react';
import styles from './ExpandableImage.module.css';

interface ExpandableImageProps {
  src: string;
  alt: string;
}

export default function ExpandableImage({ src, alt }: ExpandableImageProps) {
  const [isExpanded, setIsExpanded] = useState(false);

  return (
    <>
      <img
        src={src}
        alt={alt}
        className={styles.thumbnail}
        onClick={() => setIsExpanded(true)}
      />
      {isExpanded && (
        <div className={styles.overlay} onClick={() => setIsExpanded(false)}>
          <div className={styles.modal}>
            <button className={styles.closeBtn} onClick={() => setIsExpanded(false)}>
              ×
            </button>
            <img src={src} alt={alt} className={styles.expandedImage} />
          </div>
        </div>
      )}
    </>
  );
}
